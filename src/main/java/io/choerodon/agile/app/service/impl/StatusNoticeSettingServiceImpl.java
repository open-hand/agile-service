package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.app.assembler.StatusNoticeSettingAssembler;
import io.choerodon.agile.app.service.OrganizationConfigService;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.app.service.StatusNoticeSettingService;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.StatusNoticeSettingDTO;
import io.choerodon.agile.infra.enums.StatusNoticeUserType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.SendMsgUtil;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

/**
 * 邮件通知应用服务默认实现
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusNoticeSettingServiceImpl implements StatusNoticeSettingService {

    @Autowired
    private StatusNoticeSettingMapper statusNoticeSettingMapper;
    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private FieldValueMapper fieldValueMapper;
    @Autowired
    private StatusNoticeSettingAssembler statusNoticeSettingAssembler;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private OrganizationConfigService organizationConfigService;
    @Autowired
    private StarBeaconMapper starBeaconMapper;
    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;
    @Autowired
    private IssueUserRelMapper issueUserRelMapper;

    @Override
    public StatusNoticeSettingVO detail(Long projectId, Long issueTypeId, Long statusId, String schemeCode) {
        StatusNoticeSettingVO statusNoticeSettingVO = new StatusNoticeSettingVO(projectId, issueTypeId, statusId);
        StatusNoticeSettingDTO notice = new StatusNoticeSettingDTO(projectId, issueTypeId, statusId);
        List<StatusNoticeSettingDTO> list = statusNoticeSettingMapper.select(notice);
        if (CollectionUtils.isEmpty(list)){
            return statusNoticeSettingVO;
        }
        list.forEach(item -> statusNoticeSettingVO.addUserWithNotice(item.getUserType(), item.getUserId()));
        statusNoticeSettingVO.setNoticeTypeList(Stream.of(StringUtils.split(list.stream().map(StatusNoticeSettingDTO::getNoticeType)
                .findFirst().orElse(""), BaseConstants.Symbol.COMMA)).collect(Collectors.toList()));
        statusNoticeSettingAssembler.addUserInfo(statusNoticeSettingVO, schemeCode, issueTypeId);
        return statusNoticeSettingVO;
    }

    @Override
    public void save(Long projectId, StatusNoticeSettingVO statusNoticeSettingVO, String applyType) {
        Assert.notNull(statusNoticeSettingVO.getIssueTypeId(), BaseConstants.ErrorCode.NOT_NULL);
        Assert.notNull(statusNoticeSettingVO.getProjectId(), BaseConstants.ErrorCode.NOT_NULL);
        Assert.notNull(statusNoticeSettingVO.getStatusId(), BaseConstants.ErrorCode.NOT_NULL);
        StatusNoticeSettingDTO noticeDTO = new StatusNoticeSettingDTO(projectId, statusNoticeSettingVO.getIssueTypeId(),
                statusNoticeSettingVO.getStatusId());
        // 删除
        List<StatusNoticeSettingDTO> deleteList = statusNoticeSettingMapper.select(noticeDTO);
        if (CollectionUtils.isNotEmpty(deleteList)){
            deleteList.forEach(item -> statusNoticeSettingMapper.delete(item));
        }
        // 插入
        if (CollectionUtils.isNotEmpty(statusNoticeSettingVO.getNoticeTypeList())
                && statusNoticeSettingVO.getNoticeTypeList().size() == 1
                && statusNoticeSettingVO.getNoticeTypeList().contains("WEB_HOOK")) {
            StatusNoticeSettingDTO save = new StatusNoticeSettingDTO(statusNoticeSettingVO, "");
            statusNoticeSettingMapper.insertSelective(save);
        } else if (CollectionUtils.isNotEmpty(statusNoticeSettingVO.getNoticeTypeList()) &&
                CollectionUtils.isNotEmpty(statusNoticeSettingVO.getUserTypeList())){
            List<StatusNoticeSettingDTO> saveList = statusNoticeSettingVO.getUserTypeList()
                    .stream()
                    .filter(useType -> !StringUtils.equals(useType, StatusNoticeUserType.SPECIFIER))
                    .map(useType -> new StatusNoticeSettingDTO(statusNoticeSettingVO, useType))
                    .collect(Collectors.toList());
            saveList.addAll(statusNoticeSettingVO.getUserIdList()
                    .stream()
                    .map(userId -> new StatusNoticeSettingDTO(statusNoticeSettingVO, userId))
                    .collect(Collectors.toList()));
            saveList.forEach(statusNoticeSettingMapper::insertSelective);
        }
        projectConfigService.updateNodeObjectVersionNumber(projectId,statusNoticeSettingVO.getIssueTypeId(),
                statusNoticeSettingVO.getStatusId(),statusNoticeSettingVO.getObjectVersionNumber(),
                applyType);
    }

    @Override
    public void noticeByChangeStatus(Long projectId, Long issueId) {
        // 根据issueId找到对应的issueType和status
        IssueDTO issue = issueMapper.selectByPrimaryKey(issueId);
        Assert.notNull(issue, BaseConstants.ErrorCode.DATA_NOT_EXISTS);
        // 找到通知内容
        List<StatusNoticeSettingDTO> noticeList = statusNoticeSettingMapper.select(new StatusNoticeSettingDTO(projectId,
                issue.getIssueTypeId(), issue.getStatusId()));
        // 根据类型找到接收人
        Set<Long> userSet = new HashSet<>();
        noticeList.forEach(noticeDTO -> this.receiverType2User(projectId, noticeDTO, issue, userSet));
        userSet.removeIf(Objects::isNull);
        // 根据通知类型发消息
        sendMsgUtil.noticeIssueStatus(projectId, userSet, new ArrayList<>(Arrays.asList(StringUtils.split(noticeList.stream()
                        .map(StatusNoticeSettingDTO::getNoticeType).findFirst().orElse(""), BaseConstants.Symbol.COMMA))),
                issue, DetailsHelper.getUserDetails());
    }

    @Override
    public StatusNoticeSettingVO selectNoticeUserAndType(Long projectId, IssueDTO issue) {
        Assert.notNull(issue, BaseConstants.ErrorCode.DATA_NOT_EXISTS);
        // 找到通知内容
        List<StatusNoticeSettingDTO> noticeList = statusNoticeSettingMapper.select(new StatusNoticeSettingDTO(projectId,
                issue.getIssueTypeId(), issue.getStatusId()));
        // 根据类型找到接收人
        Set<Long> userSet = new HashSet<>();
        noticeList.forEach(noticeDTO -> this.receiverType2User(projectId, noticeDTO, issue, userSet));
        StatusNoticeSettingVO result = new StatusNoticeSettingVO();
        result.setUserIdList(userSet);
        result.setUserTypeList(new HashSet<>(Arrays.asList(StringUtils.split(noticeList.stream()
                .map(StatusNoticeSettingDTO::getNoticeType).findFirst().orElse(""), BaseConstants.Symbol.COMMA))));
        return result;
    }

    @Override
    public List<StatusNoticeSettingVO> list(Long projectId, Long issueTypeId, List<Long> statusIdList, String schemeCode) {
        if (Objects.isNull(projectId) || Objects.isNull(issueTypeId) || CollectionUtils.isEmpty(statusIdList)){
            return Collections.emptyList();
        }
        List<StatusNoticeSettingDTO> list =
                statusNoticeSettingMapper.selectByCondition(Condition.builder(StatusNoticeSettingDTO.class)
                .andWhere(Sqls.custom().andIn(StatusNoticeSettingDTO.FIELD_STATUS_ID, statusIdList)
                        .andEqualTo(StatusNoticeSettingDTO.FIELD_PROJECT_ID, projectId)
                        .andEqualTo(StatusNoticeSettingDTO.FIELD_ISSUE_TYPE_ID, issueTypeId)).build());
        return statusNoticeSettingAssembler.statusNoticeDto2Vo(projectId, issueTypeId, list, schemeCode);
    }

    @Override
    public StatusNoticeSettingVO statusNoticeDetail(Long organizationId, Long issueTypeId, Long statusId, String schemeCode) {
        StatusNoticeSettingVO statusNoticeSettingVO = new StatusNoticeSettingVO();
        statusNoticeSettingVO.setOrganizationId(organizationId);
        statusNoticeSettingVO.setIssueTypeId(issueTypeId);
        statusNoticeSettingVO.setStatusId(statusId);
        statusNoticeSettingVO.setProjectId(0L);
        StatusNoticeSettingDTO notice = new StatusNoticeSettingDTO();
        notice.setOrganizationId(organizationId);
        notice.setIssueTypeId(issueTypeId);
        notice.setStatusId(statusId);
        notice.setProjectId(0L);
        List<StatusNoticeSettingDTO> list = statusNoticeSettingMapper.select(notice);
        if (CollectionUtils.isEmpty(list)){
            return statusNoticeSettingVO;
        }
        list.forEach(item -> statusNoticeSettingVO.addUserWithNotice(item.getUserType(), item.getUserId()));
        statusNoticeSettingVO.setNoticeTypeList(Stream.of(StringUtils.split(list.stream().map(StatusNoticeSettingDTO::getNoticeType)
                .findFirst().orElse(""), BaseConstants.Symbol.COMMA)).collect(Collectors.toList()));
        statusNoticeSettingAssembler.addUserInfo(statusNoticeSettingVO, schemeCode, issueTypeId);
        return statusNoticeSettingVO;
    }

    @Override
    public void saveStatusNotice(Long organizationId, StatusNoticeSettingVO statusNoticeSettingVO) {
        Assert.notNull(statusNoticeSettingVO.getIssueTypeId(), BaseConstants.ErrorCode.NOT_NULL);
        Assert.notNull(statusNoticeSettingVO.getStatusId(), BaseConstants.ErrorCode.NOT_NULL);
        StatusNoticeSettingDTO noticeDTO = new StatusNoticeSettingDTO();
        noticeDTO.setOrganizationId(organizationId);
        noticeDTO.setIssueTypeId(statusNoticeSettingVO.getIssueTypeId());
        noticeDTO.setStatusId(statusNoticeSettingVO.getStatusId());
        noticeDTO.setProjectId(0L);
        // 删除
        List<StatusNoticeSettingDTO> deleteList = statusNoticeSettingMapper.select(noticeDTO);
        if (CollectionUtils.isNotEmpty(deleteList)){
            deleteList.forEach(item -> statusNoticeSettingMapper.delete(item));
        }
        statusNoticeSettingVO.setProjectId(0L);
        statusNoticeSettingVO.setOrganizationId(organizationId);
        // 插入
        if (CollectionUtils.isNotEmpty(statusNoticeSettingVO.getNoticeTypeList())
                && statusNoticeSettingVO.getNoticeTypeList().size() == 1
                && statusNoticeSettingVO.getNoticeTypeList().contains("WEB_HOOK")) {
            StatusNoticeSettingDTO save = new StatusNoticeSettingDTO(statusNoticeSettingVO, "");
            statusNoticeSettingMapper.insertSelective(save);
        } else if (CollectionUtils.isNotEmpty(statusNoticeSettingVO.getNoticeTypeList()) &&
                CollectionUtils.isNotEmpty(statusNoticeSettingVO.getUserTypeList())){
            List<StatusNoticeSettingDTO> saveList = statusNoticeSettingVO.getUserTypeList()
                    .stream()
                    .filter(useType -> !StringUtils.equals(useType, StatusNoticeUserType.SPECIFIER))
                    .map(useType -> new StatusNoticeSettingDTO(statusNoticeSettingVO, useType))
                    .collect(Collectors.toList());
            saveList.addAll(statusNoticeSettingVO.getUserIdList()
                    .stream()
                    .map(userId -> new StatusNoticeSettingDTO(statusNoticeSettingVO, userId))
                    .collect(Collectors.toList()));
            saveList.forEach(statusNoticeSettingMapper::insertSelective);
        }
        organizationConfigService.updateNodeObjectVersionNumber(organizationId,statusNoticeSettingVO.getIssueTypeId(),
                statusNoticeSettingVO.getStatusId(),statusNoticeSettingVO.getObjectVersionNumber());
    }

    @Override
    public List<StatusNoticeSettingVO> listStatusNoticeSetting(Long organizationId, Long issueTypeId, List<Long> statusIdList, String schemeCode) {
        if (Objects.isNull(organizationId) || Objects.isNull(issueTypeId) || CollectionUtils.isEmpty(statusIdList)){
            return Collections.emptyList();
        }
        List<StatusNoticeSettingDTO> list =
                statusNoticeSettingMapper.selectByCondition(Condition.builder(StatusNoticeSettingDTO.class)
                        .andWhere(Sqls.custom().andIn(StatusNoticeSettingDTO.FIELD_STATUS_ID, statusIdList)
                                .andEqualTo(StatusNoticeSettingDTO.FIELD_PROJECT_ID, 0L)
                                .andEqualTo(StatusNoticeSettingDTO.FIELD_ORGANIZATION_ID, organizationId)
                                .andEqualTo(StatusNoticeSettingDTO.FIELD_ISSUE_TYPE_ID, issueTypeId)).build());
        return statusNoticeSettingAssembler.dto2Vo(organizationId, issueTypeId, list, schemeCode);
    }

    private void receiverType2User(Long projectId, StatusNoticeSettingDTO noticeDTO, IssueDTO issue, Set<Long> userSet) {
        switch (noticeDTO.getUserType()){
            case StatusNoticeUserType.PROJECT_OWNER:
                userSet.addAll(baseFeignClient.listProjectOwnerById(projectId).getBody().stream().map(UserVO::getId).collect(Collectors.toSet()));
                break;
            case StatusNoticeUserType.ASSIGNEE:
                userSet.add(issue.getAssigneeId());
                break;
            case StatusNoticeUserType.REPORTER:
                userSet.add(issue.getReporterId());
                break;
            case StatusNoticeUserType.MAIN_RESPONSIBLE:
                userSet.add(issue.getMainResponsibleId());
                break;
            case StatusNoticeUserType.SPECIFIER:
                userSet.add(noticeDTO.getUserId());
                break;
            case StatusNoticeUserType.STAR_USER:
                //状态机通知增加关注人
                userSet.addAll(starBeaconMapper.selectUsersByInstanceId(projectId, issue.getIssueId()));
                break;
            case StatusNoticeUserType.ONLY_WEB_HOOK:
                break;
            case StatusNoticeUserType.PARTICIPANT:
                List<Long> participantIds = issueParticipantRelMapper.listByIssueId(projectId, issue.getIssueId());
                userSet.addAll(participantIds);
                break;
            case StatusNoticeUserType.RELATED_PARTIES:
                List<Long> relatedParties = issueUserRelMapper.listUserIdsByIssueId(projectId, issue.getIssueId(), "relatedParties");
                userSet.addAll(relatedParties);
            default:
                // 不在默认配置里，则检索自定义字段，有则加入，没有则忽略
                userSet.addAll(fieldValueMapper.selectUserIdByField(projectId, Collections.singletonList(noticeDTO.getUserType()), issue.getIssueId()));
                break;
        }
    }
}
