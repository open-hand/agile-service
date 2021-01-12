package io.choerodon.agile.app.service.impl;


import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.IssueCommentAssembler;
import io.choerodon.agile.app.service.IIssueCommentService;
import io.choerodon.agile.app.service.IssueCommentService;
import io.choerodon.agile.infra.dto.IssueCommentDTO;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueCommentMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 敏捷开发Issue评论
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:59:45
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueCommentServiceImpl implements IssueCommentService {

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
        IssueCommentVO issueCommentVO = queryByProjectIdAndCommentId(projectId, iIssueCommentService.createBase(issueCommentDTO).getCommentId());
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueCommentVO.getIssueId());
        sendMsgUtil.sendMsgByIssueComment(projectId, issueDTO, issueCommentVO);
        return issueCommentVO;
    }

    @Override
    public IssueCommentVO updateIssueComment(IssueCommentUpdateVO issueCommentUpdateVO, List<String> fieldList, Long projectId) {
        if (fieldList != null && !fieldList.isEmpty()) {
            IssueCommentDTO issueCommentDTO = issueCommentAssembler.toTarget(issueCommentUpdateVO, IssueCommentDTO.class);
            iIssueCommentService.updateBase(issueCommentDTO, fieldList.toArray(new String[fieldList.size()]));
            IssueCommentVO issueCommentVO = queryByProjectIdAndCommentId(projectId, issueCommentDTO.getCommentId());
            IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueCommentVO.getIssueId());
            if (issueCommentVO.getReplyToUserId() == null){
                sendMsgUtil.sendMsgByIssueComment(projectId, issueDTO, issueCommentVO);
            } else {
                sendMsgUtil.sendMsgByIssueCommentReplay(projectId, issueDTO, issueCommentVO);
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
        Map<Long, IssueCommentVO> commentMap = new HashMap<>(issueCommentVOList.size());
        issueCommentVOList.forEach(issueCommentVO -> {
            UserMessageDTO commentUser = userMessageMap.get(issueCommentVO.getUserId());
            issueCommentVO.setUserName(commentUser != null ? commentUser.getName() : null);
            issueCommentVO.setUserImageUrl(commentUser != null ? commentUser.getImageUrl() : null);
            issueCommentVO.setUserRealName(commentUser != null ? commentUser.getRealName() : null);
            issueCommentVO.setUserLoginName(commentUser != null ? commentUser.getLoginName() : null);
            issueCommentVO.setReplaySize(0);
            commentMap.put(issueCommentVO.getCommentId(), issueCommentVO);
            if (issueCommentVO.getParentId() != null
                    && issueCommentVO.getParentId() != 0L
                    && !ObjectUtils.isEmpty(commentMap.get(issueCommentVO.getParentId()))) {
                //设置被回复人信息
                IssueCommentVO parentComment = commentMap.get(issueCommentVO.getParentId());
                parentComment.setReplaySize(parentComment.getReplaySize() + 1);
                issueCommentVO.setReplyToUserId(parentComment.getUserId());
                issueCommentVO.setReplyToUserName(parentComment.getUserName());
                issueCommentVO.setReplyToUserLoginName(parentComment.getUserLoginName());
                issueCommentVO.setReplyToUserRealName(parentComment.getUserRealName());
                issueCommentVO.setReplyToUserImageUrl(parentComment.getUserImageUrl());
            }
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
    public int deleteIssueComment(Long projectId, Long commentId) {
        IssueCommentDTO issueCommentDTO = getCommentById(projectId, commentId);
        return iIssueCommentService.deleteBase(issueCommentDTO);
    }

    @Override
    public int deleteByIssueId(Long issueId) {
        IssueCommentDTO issueCommentDTO = new IssueCommentDTO();
        issueCommentDTO.setIssueId(issueId);
        return issueCommentMapper.delete(issueCommentDTO);
    }

    @Override
    public IssueCommentVO createIssueCommentReplay(Long projectId, IssueCommentReplayCreateVO issueCommentReplayCreateVO) {
        IssueCommentDTO issueCommentDTO = issueCommentAssembler.toTarget(issueCommentReplayCreateVO, IssueCommentDTO.class);
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        issueCommentDTO.setUserId(customUserDetails.getUserId());
        issueCommentDTO.setProjectId(projectId);
        IssueCommentVO issueCommentVO = queryByProjectIdAndCommentId(projectId, iIssueCommentService.createBase(issueCommentDTO).getCommentId());
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueCommentVO.getIssueId());
        sendMsgUtil.sendMsgByIssueCommentReplay(projectId, issueDTO, issueCommentVO);
        return issueCommentVO;
    }

    @Override
    public List<IssueCommentReplayVO> queryIssueCommentReplayList(Long projectId, Long commentId) {
        IssueCommentDTO parentIssueComment = issueCommentMapper.selectByPrimaryKey(commentId);
        if (ObjectUtils.isEmpty(parentIssueComment)) {
            throw new CommonException("error.IssueCommentRule.issueComment");
        }

        IssueCommentDTO issueCommentDTO = new IssueCommentDTO();
        issueCommentDTO.setProjectId(projectId);
        issueCommentDTO.setParentId(commentId);
        List<IssueCommentReplayVO> issueCommentVOList = modelMapper.map(
                issueCommentMapper.selectIssueCommentDesByParentId(projectId, commentId), new TypeToken<List<IssueCommentReplayVO>>(){}.getType());
        if (CollectionUtils.isEmpty(issueCommentVOList)) {
            return new ArrayList<>();
        }

        List<Long> userIds = new ArrayList<>();
        userIds.add(parentIssueComment.getUserId());
        issueCommentVOList.forEach(issueCommentVO -> userIds.add(issueCommentVO.getUserId()));
        Map<Long, UserMessageDTO> userMessageMap = userService.queryUsersMap(
                userIds.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList()), true);
        UserMessageDTO parentUser = userMessageMap.get(parentIssueComment.getUserId());

        issueCommentVOList.forEach(issueCommentVO -> {
            UserMessageDTO commentUser = userMessageMap.get(issueCommentVO.getUserId());
            issueCommentVO.setUserName(commentUser != null ? commentUser.getName() : null);
            issueCommentVO.setUserImageUrl(commentUser != null ? commentUser.getImageUrl() : null);
            issueCommentVO.setUserRealName(commentUser != null ? commentUser.getRealName() : null);
            issueCommentVO.setUserLoginName(commentUser != null ? commentUser.getLoginName() : null);
            //设置被回复人信息
            issueCommentVO.setReplyToUserId(parentIssueComment.getUserId());
            issueCommentVO.setReplyToUserName(parentUser != null ? parentUser.getImageUrl() : null);
            issueCommentVO.setReplyToUserLoginName(parentUser != null ? parentUser.getRealName() : null);
            issueCommentVO.setReplyToUserRealName(parentUser != null ? parentUser.getRealName() : null);
            issueCommentVO.setReplyToUserImageUrl(parentUser != null ? parentUser.getLoginName() : null);
        });
        return issueCommentVOList;
    }

    @Override
    public void deleteIssueCommentReplay(Long projectId, Long commentId) {
        IssueCommentDTO issueCommentDTO = getCommentById(projectId, commentId);
        iIssueCommentService.deleteBaseReplay(issueCommentDTO);
    }

    private IssueCommentVO queryByProjectIdAndCommentId(Long projectId, Long commentId) {
        IssueCommentDTO issueCommentDTO = new IssueCommentDTO();
        issueCommentDTO.setProjectId(projectId);
        issueCommentDTO.setCommentId(commentId);
        IssueCommentVO issueCommentVO = modelMapper.map(issueCommentMapper.selectOne(issueCommentDTO), IssueCommentVO.class);
        UserDTO userDTO = userService.queryUserNameByOption(issueCommentVO.getUserId(), true);
        issueCommentVO.setUserName(userDTO.getRealName());
        issueCommentVO.setUserImageUrl(userDTO.getImageUrl());
        if (issueCommentVO.getParentId() != null && issueCommentVO.getParentId() != 0L) {
            //如果父评论不为空，则设置父评论的user信息至被回复人
            IssueCommentDTO parentCommentRecord = new IssueCommentDTO();
            parentCommentRecord.setProjectId(projectId);
            parentCommentRecord.setCommentId(issueCommentVO.getParentId() );
            IssueCommentDTO parentCommentDTO = issueCommentMapper.selectOne(parentCommentRecord);
            UserDTO parentUserDTO = userService.queryUserNameByOption(parentCommentDTO.getUserId(), true);
            issueCommentVO.setReplyToUserId(parentCommentDTO.getUserId());
            issueCommentVO.setReplyToUserName(parentUserDTO.getRealName());
            issueCommentVO.setReplyToUserImageUrl(parentUserDTO.getImageUrl());
        }
        return issueCommentVO;
    }


}