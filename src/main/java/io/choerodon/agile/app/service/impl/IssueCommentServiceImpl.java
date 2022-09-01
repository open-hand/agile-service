package io.choerodon.agile.app.service.impl;


import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.IssueCommentAssembler;
import io.choerodon.agile.app.service.IIssueCommentService;
import io.choerodon.agile.app.service.IssueCommentService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.IssueCommentDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.mapper.IssueCommentMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * 敏捷开发Issue评论
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:59:45
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueCommentServiceImpl implements IssueCommentService {

    private static final int DISPALY_REPLY_SIZE = 2;

    @Autowired
    private IssueCommentAssembler issueCommentAssembler;
    @Autowired
    private IssueCommentMapper issueCommentMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IIssueCommentService iIssueCommentService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private SendMsgUtil sendMsgUtil;

    @Override
    public IssueCommentVO createIssueComment(Long projectId, IssueCommentCreateVO issueCommentCreateVO) {
        IssueCommentDTO issueCommentDTO = issueCommentAssembler.toTarget(issueCommentCreateVO, IssueCommentDTO.class);
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        issueCommentDTO.setUserId(customUserDetails.getUserId());
        issueCommentDTO.setProjectId(projectId);
        issueCommentDTO.setParentId(0L);
        issueCommentDTO.setReplyToUserId(0L);
        IssueCommentVO issueCommentVO = queryByProjectIdAndCommentId(projectId, iIssueCommentService.createBase(issueCommentDTO).getCommentId());
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueCommentVO.getIssueId());
        sendMsgUtil.sendMsgByIssueComment(projectId, issue, issueCommentVO, DetailsHelper.getUserDetails().getUserId());
        return issueCommentVO;
    }

    @Override
    public IssueCommentVO updateIssueComment(IssueCommentUpdateVO issueCommentUpdateVO, List<String> fieldList, Long projectId) {
        if (fieldList != null && !fieldList.isEmpty()) {
            IssueCommentDTO issueCommentDTO = issueCommentAssembler.toTarget(issueCommentUpdateVO, IssueCommentDTO.class);
            iIssueCommentService.updateBase(issueCommentDTO, fieldList.toArray(new String[fieldList.size()]));
            IssueCommentVO issueCommentVO = queryByProjectIdAndCommentId(projectId, issueCommentDTO.getCommentId());

            IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueCommentVO.getIssueId());
            Long operatorId = DetailsHelper.getUserDetails().getUserId();
            if (issueCommentVO.getReplyToUserId() == null || issueCommentVO.getReplyToUserId() == 0L) {
                sendMsgUtil.sendMsgByIssueComment(projectId, issue, issueCommentVO, operatorId);
            } else if (!issueCommentVO.getReplyToUserId().equals(operatorId)) {
                sendMsgUtil.sendMsgByIssueCommentReply(projectId, issue, issueCommentVO, operatorId);
            }
            return issueCommentVO;
        } else {
            return null;
        }
    }

    @Override
    public List<IssueCommentVO> queryIssueCommentList(Long projectId, Long issueId) {
        List<IssueCommentVO> issueCommentVOList = modelMapper.map(issueCommentMapper.queryIssueCommentList(projectId, issueId), new TypeToken<List<IssueCommentVO>>(){}.getType());
        if (CollectionUtils.isEmpty(issueCommentVOList)) {
            return new ArrayList<>();
        }
        List<Long> userIds = new ArrayList<>();
        issueCommentVOList.forEach(issueCommentVO -> userIds.add(issueCommentVO.getUserId()));
        Map<Long, UserMessageDTO> userMessageMap = userService.queryUsersMap(
                userIds.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList()), true);
        issueCommentVOList.forEach(issueCommentVO -> {
            UserMessageDTO commentUser = userMessageMap.get(issueCommentVO.getUserId());
            issueCommentVO.setUserName(commentUser != null ? commentUser.getName() : null);
            issueCommentVO.setUserImageUrl(commentUser != null ? commentUser.getImageUrl() : null);
            issueCommentVO.setUserRealName(commentUser != null ? commentUser.getRealName() : null);
            issueCommentVO.setUserLoginName(commentUser != null ? commentUser.getLoginName() : null);

            IssueCommentDTO childSizeRecord = new IssueCommentDTO();
            childSizeRecord.setProjectId(projectId);
            childSizeRecord.setIssueId(issueId);
            childSizeRecord.setParentId(issueCommentVO.getCommentId());
            issueCommentVO.setReplySize(issueCommentMapper.selectCount(childSizeRecord));
        });
        return issueCommentVOList;
    }

    private IssueCommentDTO getCommentById(Long projectId, Long commentId) {
        IssueCommentDTO issueCommentDTO = new IssueCommentDTO();
        issueCommentDTO.setProjectId(projectId);
        issueCommentDTO.setCommentId(commentId);
        issueCommentDTO = issueCommentMapper.selectOne(issueCommentDTO);
        if (issueCommentDTO == null) {
            throw new CommonException("error.comment.get");
        }
        return issueCommentDTO;
    }

    @Override
    public int deleteIssueComment(Long projectId, Long commentId, boolean self) {
        IssueCommentDTO issueCommentDTO = getCommentById(projectId, commentId);
        if (self && !DetailsHelper.getUserDetails().getUserId().equals(issueCommentDTO.getUserId())) {
            throw new CommonException("error.created.user.illegal");
        }
        return iIssueCommentService.deleteBase(issueCommentDTO);
    }

    @Override
    public int deleteByIssueId(Long issueId) {
        IssueCommentDTO issueCommentDTO = new IssueCommentDTO();
        issueCommentDTO.setIssueId(issueId);
        return issueCommentMapper.delete(issueCommentDTO);
    }

    @Override
    public IssueCommentVO createIssueCommentReply(Long projectId, IssueCommentReplyCreateVO issueCommentReplyCreateVO) {
        IssueCommentDTO issueCommentDTO = issueCommentAssembler.toTarget(issueCommentReplyCreateVO, IssueCommentDTO.class);
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        issueCommentDTO.setUserId(customUserDetails.getUserId());
        issueCommentDTO.setProjectId(projectId);
        IssueCommentVO issueCommentVO = queryByProjectIdAndCommentId(projectId, iIssueCommentService.createBase(issueCommentDTO).getCommentId());
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueCommentVO.getIssueId());
        if (!issueCommentVO.getReplyToUserId().equals(customUserDetails.getUserId())) {
            sendMsgUtil.sendMsgByIssueCommentReply(projectId, issue, issueCommentVO, customUserDetails.getUserId());
        }
        return issueCommentVO;
    }

    @Override
    public List<IssueCommentReplyVO> queryIssueCommentReplyList(Long projectId, Long commentId) {
        IssueCommentDTO parentIssueComment = issueCommentMapper.selectByPrimaryKey(commentId);
        if (ObjectUtils.isEmpty(parentIssueComment)) {
            throw new CommonException("error.IssueCommentRule.issueComment");
        }

        IssueCommentDTO issueCommentDTO = new IssueCommentDTO();
        issueCommentDTO.setProjectId(projectId);
        issueCommentDTO.setParentId(commentId);
        List<IssueCommentReplyVO> issueCommentVOList = modelMapper.map(
                issueCommentMapper.selectIssueCommentDesByParentId(projectId, commentId), new TypeToken<List<IssueCommentReplyVO>>(){}.getType());
        if (CollectionUtils.isEmpty(issueCommentVOList)) {
            return new ArrayList<>();
        }

        List<Long> userIds = new ArrayList<>();
        issueCommentVOList.forEach(issueCommentVO -> {
            userIds.add(issueCommentVO.getUserId());
            userIds.add(issueCommentVO.getReplyToUserId());
        });
        Map<Long, UserMessageDTO> userMessageMap = userService.queryUsersMap(
                userIds.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList()), true);

        issueCommentVOList.forEach(issueCommentVO -> {
            UserMessageDTO replyToUser = userMessageMap.get(issueCommentVO.getReplyToUserId());
            UserMessageDTO commentUser = userMessageMap.get(issueCommentVO.getUserId());
            if (!ObjectUtils.isEmpty(commentUser)) {
                issueCommentVO.setUserName(commentUser.getName());
                issueCommentVO.setUserImageUrl(commentUser.getImageUrl());
                issueCommentVO.setUserRealName(commentUser.getRealName());
                issueCommentVO.setUserLoginName(commentUser.getLoginName());
            }
            //设置被回复人信息
            issueCommentVO.setReplyToUserId(parentIssueComment.getUserId());
            if (!ObjectUtils.isEmpty(replyToUser)) {
                issueCommentVO.setReplyToUserName(replyToUser.getName());
                issueCommentVO.setReplyToUserLoginName(replyToUser.getLoginName());
                issueCommentVO.setReplyToUserRealName(replyToUser.getRealName());
                issueCommentVO.setReplyToUserImageUrl(replyToUser.getImageUrl());
            }
        });
        return issueCommentVOList;
    }

    @Override
    public void deleteIssueCommentReply(Long projectId, Long commentId, boolean self) {
        IssueCommentDTO issueCommentDTO = getCommentById(projectId, commentId);
        if (self && !DetailsHelper.getUserDetails().getUserId().equals(issueCommentDTO.getUserId())) {
            throw new CommonException("error.created.user.illegal");
        }
        iIssueCommentService.deleteBaseReply(issueCommentDTO);
    }

    @Override
    public Page<IssueCommentVO> queryIssueCommentPage(PageRequest pageRequest, Long issueId, Long projectId) {
        Page<IssueCommentDTO> issueCommentList = PageHelper.doPage(pageRequest, () -> issueCommentMapper.queryIssueCommentList(projectId, issueId));
        if (CollectionUtils.isEmpty(issueCommentList)){
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
        Page<IssueCommentVO> result = PageUtils.copyPropertiesAndResetContent(issueCommentList, modelMapper.map(issueCommentList.getContent(), new TypeToken<List<IssueCommentVO>>(){}.getType()));
        setDefaultDisplayReply(result, issueId, projectId);

        List<Long> userIds = new ArrayList<>();
        result.forEach(issueCommentVO -> {
            userIds.add(issueCommentVO.getUserId());
            if (!CollectionUtils.isEmpty(issueCommentVO.getIssueCommentReplyList())){
                issueCommentVO.getIssueCommentReplyList().forEach(issueCommentReply -> {
                    userIds.add(issueCommentReply.getUserId());
                    userIds.add(issueCommentReply.getReplyToUserId());
                });
            }
        });
        Map<Long, UserMessageDTO> userMessageMap = userService.queryUsersMap(
                userIds.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList()), true);
        result.forEach(issueCommentVO -> {
            UserMessageDTO commentUser = userMessageMap.get(issueCommentVO.getUserId());
            issueCommentVO.setUserName(commentUser != null ? commentUser.getName() : null);
            issueCommentVO.setUserImageUrl(commentUser != null ? commentUser.getImageUrl() : null);
            issueCommentVO.setUserRealName(commentUser != null ? commentUser.getRealName() : null);
            issueCommentVO.setUserLoginName(commentUser != null ? commentUser.getLoginName() : null);
            if (!CollectionUtils.isEmpty(issueCommentVO.getIssueCommentReplyList())){
                issueCommentVO.getIssueCommentReplyList().forEach(issueCommentReply -> {
                    setReplyUserInfo(issueCommentReply, userMessageMap);
                });
            }
        });

        return result;
    }

    private void setReplyUserInfo(IssueCommentReplyVO issueCommentReply, Map<Long, UserMessageDTO> userMessageMap) {
        UserMessageDTO replyToUser = userMessageMap.get(issueCommentReply.getReplyToUserId());
        UserMessageDTO commentReplyUser = userMessageMap.get(issueCommentReply.getUserId());
        if (!ObjectUtils.isEmpty(commentReplyUser)) {
            issueCommentReply.setUserName(commentReplyUser.getName());
            issueCommentReply.setUserImageUrl(commentReplyUser.getImageUrl());
            issueCommentReply.setUserRealName(commentReplyUser.getRealName());
            issueCommentReply.setUserLoginName(commentReplyUser.getLoginName());
        }
        if (!ObjectUtils.isEmpty(replyToUser)) {
            issueCommentReply.setReplyToUserName(replyToUser.getName());
            issueCommentReply.setReplyToUserLoginName(replyToUser.getLoginName());
            issueCommentReply.setReplyToUserRealName(replyToUser.getRealName());
            issueCommentReply.setReplyToUserImageUrl(replyToUser.getImageUrl());
        }
    }

    private void setDefaultDisplayReply(Page<IssueCommentVO> result, Long issueId, Long projectId) {
        if (CollectionUtils.isEmpty(result.getContent())){
            return;
        }
        Map<Long, IssueCommentVO> commentMap = new HashMap<>(result.size());
        Set<Long> commentIds = new HashSet<>();
        result.forEach(issueCommentVO -> {
            issueCommentVO.setIssueCommentReplyList(new ArrayList<>());
            issueCommentVO.setReplySize(0);
            commentMap.put(issueCommentVO.getCommentId(), issueCommentVO);
            commentIds.add(issueCommentVO.getCommentId());
        });
        List<IssueCommentReplyVO> commentReplyList = issueCommentMapper.selectIssueCommentDesByParentIds(commentIds, issueId, projectId);
        if (!CollectionUtils.isEmpty(commentReplyList)) {
            commentReplyList.forEach(commentReply ->
                    commentMap.computeIfPresent(commentReply.getParentId(), (commentId, issueCommentVO) -> {
                        int size = issueCommentVO.getReplySize();
                        if (size < DISPALY_REPLY_SIZE) {
                            issueCommentVO.getIssueCommentReplyList().add(commentReply);
                        }
                        issueCommentVO.setReplySize(size + 1);
                        return issueCommentVO;
                    }));
        }
    }

    private IssueCommentVO queryByProjectIdAndCommentId(Long projectId, Long commentId) {
        IssueCommentDTO issueCommentDTO = new IssueCommentDTO();
        issueCommentDTO.setProjectId(projectId);
        issueCommentDTO.setCommentId(commentId);
        IssueCommentVO issueCommentVO = modelMapper.map(issueCommentMapper.selectOne(issueCommentDTO), IssueCommentVO.class);

        List<Long> userIds = new ArrayList<>();
        userIds.add(issueCommentVO.getUserId());
        if (issueCommentVO.getParentId() != null && issueCommentVO.getParentId() != 0L) {
            //如果父评论不为空，则设置父评论的user信息至被回复人
            userIds.add(issueCommentVO.getReplyToUserId());
        }
        Map<Long, UserMessageDTO> userMap = userService.queryUsersMap(userIds, true);
        UserMessageDTO user = userMap.get(issueCommentVO.getUserId());
        UserMessageDTO replyToUser = userMap.get(issueCommentVO.getReplyToUserId());
        issueCommentVO.setUserName(user != null ? user.getName() : null);        issueCommentVO.setUserName(user != null ? user.getName() : null);
        issueCommentVO.setUserRealName(user != null ? user.getRealName() : null);
        issueCommentVO.setUserImageUrl(user != null ? user.getImageUrl() : null);
        issueCommentVO.setReplyToUserName(replyToUser != null ? replyToUser.getName() : null);
        issueCommentVO.setReplyToUserRealName(replyToUser != null ? replyToUser.getRealName() : null);
        issueCommentVO.setReplyToUserImageUrl(replyToUser != null ? replyToUser.getImageUrl() : null);
        return issueCommentVO;
    }

    @Override
    public void copyIssueComments(Long projectId, Long issueId, Long newIssueId) {
        List<IssueCommentDTO> issueCommentDTOS = issueCommentMapper.queryAllIssueCommentList(projectId, issueId);
        if (ObjectUtils.isEmpty(issueCommentDTOS)) {
            return;
        }
        Map<Long, List<IssueCommentDTO>> sonCommentMap = issueCommentDTOS.stream().collect(Collectors.groupingBy(IssueCommentDTO::getParentId));
        // 按照id倒序创建
        List<IssueCommentDTO> parentCommentList = sonCommentMap.get(0L);
        parentCommentList.sort(Comparator.comparing(IssueCommentDTO::getCommentId));
        parentCommentList.forEach(comment -> {
            IssueCommentDTO create = new IssueCommentDTO();
            create.setIssueId(newIssueId);
            create.setCommentText(comment.getCommentText());
            create.setUserId(comment.getUserId());
            create.setProjectId(projectId);
            create.setParentId(0L);
            create.setReplyToUserId(0L);
            IssueCommentDTO newComment = iIssueCommentService.createBase(create);
            List<IssueCommentDTO> sonCommentList = sonCommentMap.get(comment.getCommentId());
            if (ObjectUtils.isEmpty(sonCommentList)) {
                return;
            }
            sonCommentList.sort(Comparator.comparing(IssueCommentDTO::getCommentId));
            sonCommentList.forEach(sonComment -> {
                IssueCommentDTO sonCreate = new IssueCommentDTO();
                sonCreate.setIssueId(newIssueId);
                sonCreate.setCommentText(sonComment.getCommentText());
                sonCreate.setUserId(sonComment.getUserId());
                sonCreate.setProjectId(projectId);
                sonCreate.setParentId(newComment.getCommentId());
                sonCreate.setReplyToUserId(comment.getUserId());
                iIssueCommentService.createBase(sonCreate);
            });
        });
    }

}