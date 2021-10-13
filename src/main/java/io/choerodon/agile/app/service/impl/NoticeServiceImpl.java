package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.infra.enums.StatusNoticeUserType;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.assembler.NoticeMessageAssembler;
import io.choerodon.agile.app.service.NoticeService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.MessageDTO;
import io.choerodon.agile.infra.dto.MessageDetailDTO;
import io.choerodon.agile.infra.feign.vo.MessageSettingVO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/10/9.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class NoticeServiceImpl implements NoticeService {

    private static final String USERS = "specifier";
    private static final String ISSUE_CREATE = "ISSUECREATE";
    private static final String STAR_USER = "starUser";

    @Autowired
    private NoticeMapper noticeMapper;

    @Autowired
    private NoticeDetailMapper noticeDetailMapper;

    @Autowired
    private UserService userService;

    @Autowired
    private NoticeMessageAssembler noticeMessageAssembler;

    @Autowired
    private NotifyFeignClient notifyFeignClient;

    @Autowired
    private StarBeaconMapper starBeaconMapper;

    @Autowired
    private FieldValueMapper fieldValueMapper;

    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;

    private void getIds(List<MessageDTO> result, List<Long> ids) {
        for (MessageDTO messageDTO : result) {
            if (USERS.equals(messageDTO.getNoticeType()) && messageDTO.getEnable() && messageDTO.getUser() != null && messageDTO.getUser().length() != 0 && !"null".equals(messageDTO.getUser())) {
                String[] strs = messageDTO.getUser().split(",");
                for (String str : strs) {
                    Long id = Long.parseLong(str);
                    if (!ids.contains(id)) {
                        ids.add(id);
                    }
                }
            }
        }
    }

    @Override
    public List<MessageVO> queryByProjectId(Long projectId) {
        List<MessageDTO> result = new ArrayList<>();
        List<MessageDTO> originMessageList = noticeMapper.selectAll();
        List<MessageDTO> changeMessageList = noticeMapper.selectChangeMessageByProjectId(projectId);
        for (MessageDTO messageDTO : originMessageList) {
            int flag = 0;
            for (MessageDTO changeMessageDTO : changeMessageList) {
                if (messageDTO.getEvent().equals(changeMessageDTO.getEvent()) && messageDTO.getNoticeType().equals(changeMessageDTO.getNoticeType())) {
                    flag = 1;
                    result.add(changeMessageDTO);
                    break;
                }
            }
            if (flag == 0) {
                result.add(messageDTO);
            }
        }
        List<Long> ids = new ArrayList<>();
        getIds(result, ids);
        return noticeMessageAssembler.messageDTOToVO(result, ids);
    }

    @Override
    public void updateNotice(Long projectId, List<MessageVO> messageVOList) {
        for (MessageVO messageVO : messageVOList) {
            MessageDetailDTO messageDetailDTO = new MessageDetailDTO();
            messageDetailDTO.setProjectId(projectId);
            messageDetailDTO.setEnable(messageVO.getEnable());
            messageDetailDTO.setEvent(messageVO.getEvent());
            messageDetailDTO.setNoticeType(messageVO.getNoticeType());
            messageDetailDTO.setNoticeName(messageVO.getNoticeName());
            messageDetailDTO.setUser(messageVO.getUser());
            if (noticeMapper.selectChangeMessageByDetail(projectId, messageVO.getEvent(), messageVO.getNoticeType()) == null) {
                if (noticeDetailMapper.insert(messageDetailDTO) != 1) {
                    throw new CommonException("error.messageDetailDTO.insert");
                }
            } else {
                messageDetailDTO.setId(messageVO.getId());
                messageDetailDTO.setObjectVersionNumber(messageVO.getObjectVersionNumber());
                if (noticeDetailMapper.updateByPrimaryKeySelective(messageDetailDTO) != 1) {
                    throw new CommonException("error.messageDetailDTO.update");
                }
            }
        }
    }

    private void addUsersByReporter(List<String> res, List<Long> result, IssueVO issueVO) {
        if (res.contains("reporter") && !result.contains(issueVO.getReporterId())) {
            result.add(issueVO.getReporterId());
        }
    }

    private void addUsersByAssigneer(List<String> res, List<Long> result, IssueVO issueVO) {
        if (res.contains("assignee") && issueVO.getAssigneeId() != null && !result.contains(issueVO.getAssigneeId())) {
            result.add(issueVO.getAssigneeId());
        }
    }

    private void addUsersByMainResponsible(List<String> res, List<Long> result, IssueVO issueVO) {
        Long mainResponsibleId = issueVO.getMainResponsibleId();
        if (res.contains("mainResponsible") && mainResponsibleId != null && !result.contains(mainResponsibleId)) {
            result.add(mainResponsibleId);
        }
    }

    private void addUsersByProjectOwner(Long projectId, List<String> res, List<Long> result) {
        if (res.contains("projectOwner")) {
            List<UserVO> userDTOS = userService.listProjectAdminUsersByProjectId(projectId);
            if(!CollectionUtils.isEmpty(userDTOS)){
                for (UserVO userVO:userDTOS) {
                    if(!result.contains(userVO.getId())){
                        result.add(userVO.getId());
                    }
                }
            }
        }
    }

    private void addUsersByUsers (List<String> res, List<Long> result, Set<Long> users) {
        if (res.contains(USERS) && !CollectionUtils.isEmpty(users)) {
            for (Long userId : users) {
                if (!result.contains(userId)) {
                    result.add(userId);
                }
            }
        }
    }
    
    private void addUsersByStarUsers(Long projectId, List<String> res, String code, List<Long> result, IssueVO issueVO) {
        if (!Objects.equals(ISSUE_CREATE, code) && res.contains(STAR_USER)) {
            List<Long> userIds = starBeaconMapper.selectUsersByInstanceId(projectId, issueVO.getIssueId());
            if (!CollectionUtils.isEmpty(userIds)) {
                userIds.forEach(userId -> {
                    if (!result.contains(userId)) {
                        result.add(userId);
                    }
                });
            }
        }
    }

    private void addUsersByCustomUserTypes(Long projectId, List<String> res, List<Long> result, IssueVO issueVO) {
        //获取自定义人员字段编码
        List<String> customUserTypes = new ArrayList<>(res);
        customUserTypes.removeAll(Arrays.asList(StatusNoticeUserType.BASE_USER_TYPE_LIST));
        if (CollectionUtils.isEmpty(customUserTypes)) {
            return;
        }
        //添加字段人员值
        List<Long> customFieldUserIds = fieldValueMapper.selectUserIdByField(projectId, customUserTypes, issueVO.getIssueId());
        if (!CollectionUtils.isEmpty(customFieldUserIds)) {
            customFieldUserIds.forEach(userId -> {
                if (!result.contains(userId)) {
                    result.add(userId);
                }
            });
        }
    }

    @Override
    public List<Long> queryUserIdsByProjectId(Long projectId, String code, IssueVO issueVO) {
        ResponseEntity<MessageSettingVO> messageSetting = notifyFeignClient.getMessageSetting(projectId,"agile", code,null,null);
        MessageSettingVO messageVo = messageSetting.getBody();
        if(ObjectUtils.isEmpty(messageVo)){
            throw new CommonException("error.message.setting.is.null");
        }
        Set<String> type = new HashSet<>();
        Set<Long> users = new HashSet<>();
        messageVo.getTargetUserDTOS().forEach(v -> {
            type.add(v.getType());
            if (!ObjectUtils.isEmpty(v.getUserId()) && !v.getUserId().equals(0L)) {
                users.add(v.getUserId());
            }
        });
        List<String> res = type.stream().collect(Collectors.toList());
        List<Long> result = new ArrayList<>();
        addUsersByReporter(res, result, issueVO);
        addUsersByAssigneer(res, result, issueVO);
        addUsersByMainResponsible(res, result, issueVO);
        addUsersByProjectOwner(projectId, res, result);
        addUsersByUsers(res, result, users);
        //通知增加关注人
        addUsersByStarUsers(projectId, res, code, result, issueVO);
        // 通知增加参与人
        addUsersByParticipants(projectId, res, code, result, issueVO);
        //通知增加自定义人员字段选项
        addUsersByCustomUserTypes(projectId, res, result, issueVO);
        return result;
    }

    private void addUsersByParticipants(Long projectId, List<String> res, String code, List<Long> result, IssueVO issueVO) {
        if (res.contains("participant")) {
            List<Long> participantIds = issueParticipantRelMapper.listByIssueId(projectId, issueVO.getIssueId());
            if (!CollectionUtils.isEmpty(participantIds)) {
                result.addAll(participantIds);
            }
        }
    }

    @Override
    public List<MessageDetailDTO> migrateMessageDetail() {
        List<MessageDetailDTO> messageDetailDTOS = noticeDetailMapper.selectAll();
        if(CollectionUtils.isEmpty(messageDetailDTOS)){
            return new ArrayList<>();
        }
        return messageDetailDTOS;
    }

    @Override
    public List<Long> queryCustomFieldUserIdsByProjectId(Long projectId, String code, IssueVO issueVO) {
        ResponseEntity<MessageSettingVO> messageSetting = notifyFeignClient.getMessageSetting(projectId,"agile", code,null,null);
        MessageSettingVO messageVO = messageSetting.getBody();
        if(ObjectUtils.isEmpty(messageVO)){
            throw new CommonException("error.message.setting.is.null");
        }

        Set<String> type = new HashSet<>();
        Set<Long> users = new HashSet<>();
        messageVO.getTargetUserDTOS().forEach(v -> {
            type.add(v.getType());
            if (!ObjectUtils.isEmpty(v.getUserId()) && !v.getUserId().equals(0L)) {
                users.add(v.getUserId());
            }
        });
        List<String> res = new ArrayList<>(type);
        //获取已通知过的人员
        List<Long> alreadySendUsers = new ArrayList<>();
        addUsersByReporter(res, alreadySendUsers, issueVO);
        addUsersByAssigneer(res, alreadySendUsers, issueVO);
        addUsersByProjectOwner(projectId, res, alreadySendUsers);
        addUsersByUsers(res, alreadySendUsers, users);

        //获取自定义字段通知人员
        List<Long> result = new ArrayList<>();
        addUsersByCustomUserTypes(projectId, res, result, issueVO);
        //移除重复通知人员
        result.removeAll(alreadySendUsers);
        return result;
    }
}
