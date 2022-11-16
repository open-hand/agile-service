package io.choerodon.agile.app.assembler;

import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toCollection;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.enums.StatusType;
import io.choerodon.agile.infra.mapper.IssueStatusMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.DateUtil;
import io.choerodon.agile.infra.utils.ListUtil;
import io.choerodon.core.exception.CommonException;

import org.hzero.core.base.BaseConstants;

/**
 * @author dinghuang123@gmail.com
 */
@Component
public class IssueAssembler extends AbstractAssembler {

    @Autowired
    private UserService userService;
    @Autowired
    private SprintNameAssembler sprintNameAssembler;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private LookupValueService lookupValueService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private IssueTypeService issueTypeService;

    /**
     * issueDetailDO转换到IssueDTO
     *
     * @param issueDetailDTO issueDetailDTO
     * @return IssueVO
     */
    public IssueVO issueDetailDTOToVO(IssueDetailDTO issueDetailDTO, Map<Long, IssueTypeVO> issueTypeDTOMap, Map<Long, StatusVO> statusMapDTOMap, Map<Long, PriorityVO> priorityDTOMap) {
        IssueVO issueVO = new IssueVO();
        BeanUtils.copyProperties(issueDetailDTO, issueVO);
        issueVO.setComponentIssueRelVOList(modelMapper.map(issueDetailDTO.getComponentIssueRelDTOList(), new TypeToken<List<ComponentIssueRelVO>>(){}.getType()));
        issueVO.setActiveSprint(sprintNameAssembler.toTarget(issueDetailDTO.getActiveSprint(), SprintNameVO.class));
        issueVO.setCloseSprint(sprintNameAssembler.toTargetList(issueDetailDTO.getCloseSprint(), SprintNameVO.class));
        issueVO.setVersionIssueRelVOList(modelMapper.map(issueDetailDTO.getVersionIssueRelDTOList(), new TypeToken<List<VersionIssueRelVO>>(){}.getType()));
        issueVO.setLabelIssueRelVOList(modelMapper.map(issueDetailDTO.getLabelIssueRelDTOList(), new TypeToken<List<LabelIssueRelVO>>(){}.getType()));
        issueVO.setIssueAttachmentVOList(modelMapper.map(issueDetailDTO.getIssueAttachmentDTOList(), new TypeToken<List<IssueAttachmentVO>>(){}.getType()));
        issueVO.setIssueCommentVOList(modelMapper.map(issueDetailDTO.getIssueCommentDTOList(), new TypeToken<List<IssueCommentVO>>(){}.getType()));
        issueVO.setTags(issueDetailDTO.getTags());
        issueVO.setSubIssueVOList(issueDoToSubIssueDto(issueDetailDTO.getSubIssueDTOList(), issueTypeDTOMap, statusMapDTOMap, priorityDTOMap));
        issueVO.setSubBugVOList(issueDoToSubIssueDto(issueDetailDTO.getSubBugDOList(), issueTypeDTOMap, statusMapDTOMap, priorityDTOMap));
        issueVO.setSameParentIssueVOList(issueDoToSubIssueDto(issueDetailDTO.getSameParentIssueDTOList(), issueTypeDTOMap, statusMapDTOMap, priorityDTOMap));
        issueVO.setSameParentBugVOList(issueDoToSubIssueDto(issueDetailDTO.getSameParentBugDOList(), issueTypeDTOMap, statusMapDTOMap, priorityDTOMap));
        issueVO.setPriorityVO(priorityDTOMap.get(issueVO.getPriorityId()));
        issueVO.setIssueTypeVO(issueTypeDTOMap.get(issueVO.getIssueTypeId()));
        issueVO.setStatusVO(statusMapDTOMap.get(issueVO.getStatusId()));
        issueVO.setProjectVO(ConvertUtil.queryProject(issueVO.getProjectId()));
        List<Long> assigneeIdList = new ArrayList<>();
        assigneeIdList.add(issueDetailDTO.getAssigneeId());
        assigneeIdList.add(issueDetailDTO.getReporterId());
        assigneeIdList.add(issueDetailDTO.getCreatedBy());
        assigneeIdList.add(issueDetailDTO.getMainResponsibleId());
        assigneeIdList.add(issueDetailDTO.getLastUpdatedBy());
        if (!CollectionUtils.isEmpty(issueDetailDTO.getParticipantIds())) {
            assigneeIdList.addAll(issueDetailDTO.getParticipantIds());
        }
        Boolean issueCommentCondition = CollectionUtils.isNotEmpty(issueVO.getIssueCommentVOList());
        if (Boolean.TRUE.equals(issueCommentCondition)) {
            assigneeIdList.addAll(issueVO.getIssueCommentVOList().stream().map(IssueCommentVO::getUserId).collect(Collectors.toList()));
        }
        assigneeIdList = ListUtil.filterByKey(assigneeIdList, 0L);
        Map<Long, UserMessageDTO> userMessageDOMap = userService.queryUsersMap(
                assigneeIdList.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList()), true);
        handlerUser(issueVO, userMessageDOMap, issueDetailDTO);
        if (Boolean.TRUE.equals(issueCommentCondition)) {
            Map<Long, Integer> parentSizeMap = new HashMap<>(issueVO.getIssueCommentVOList().size());
            Iterator<IssueCommentVO> iterator = issueVO.getIssueCommentVOList().iterator();
            while (iterator.hasNext()) {
                IssueCommentVO issueCommentVO = iterator.next();
                if (issueCommentVO.getParentId() != null && issueCommentVO.getParentId() != 0L) {
                    Integer size = parentSizeMap.getOrDefault(issueCommentVO.getParentId(),0);
                    parentSizeMap.put(issueCommentVO.getParentId(), size + 1);
                    iterator.remove();
                } else {
                    UserMessageDTO commentUser = userMessageDOMap.get(issueCommentVO.getUserId());
                    if (!ObjectUtils.isEmpty(commentUser)) {
                        issueCommentVO.setUserName( commentUser.getName());
                        issueCommentVO.setUserImageUrl(commentUser.getImageUrl());
                        issueCommentVO.setUserRealName(commentUser.getRealName());
                        issueCommentVO.setUserLoginName(commentUser.getLoginName());
                    }
                    issueCommentVO.setReplySize(parentSizeMap.getOrDefault(issueCommentVO.getCommentId(),0));
                }
            }
        }
        return issueVO;
    }

    public List<IssueVO> issueDOToCopyIssueVOList(List<IssueDTO> issueDTOList, Long organizationId, Long projectId, List<Long> issueIds) {
        List<IssueVO> result = new ArrayList<>();
        Map<Long, List<ProductVO>> issueProductMap = new HashMap<>();
        if (agilePluginService != null) {
            issueProductMap.putAll(agilePluginService.listProductMap(organizationId, Arrays.asList(projectId), issueIds));
        }
        issueDTOList.forEach(issueDO -> {
            IssueVO issueVO = new IssueVO();
            BeanUtils.copyProperties(issueDO, issueVO);
            issueVO.setComponentIssueRelVOList(toTargetList(issueDO.getIssueComponentBriefDTOS(), ComponentIssueRelVO.class));
            issueVO.setLabelIssueRelVOList(toTargetList(issueDO.getLabelIssueRelDTOS(), LabelIssueRelVO.class));
            List<VersionIssueRelVO> versionList = toTargetList(issueDO.getVersionIssueRelDTOS(), VersionIssueRelVO.class);
            issueVO.setVersionIssueRelVOList(versionList);
            issueVO.setCloseSprint(toTargetList(issueDO.getIssueSprintDTOS(), SprintNameVO.class));
            issueVO.setProductVOList(issueProductMap.get(issueDO.getIssueId()));
            issueVO.setTags(issueDO.getTags());
            List<UserMessageDTO> participants = new ArrayList<>();
            if (!ObjectUtils.isEmpty(issueDO.getParticipantIds())) {
                issueDO.getParticipantIds().forEach(participantId -> {
                    UserMessageDTO participant = new UserMessageDTO();
                    participant.setId(participantId);
                    participants.add(participant);
                });
            }
            issueVO.setParticipants(participants);
            result.add(issueVO);
        });
        return result;
    }

    private void handlerUser(IssueVO issueVO, Map<Long, UserMessageDTO> userMessageDOMap, IssueDetailDTO issueDetailDTO) {
        UserMessageDTO assigneeUserDO = userMessageDOMap.get(issueVO.getAssigneeId());
        UserMessageDTO reporterUserDO = userMessageDOMap.get(issueVO.getReporterId());
        UserMessageDTO createrUserDO = userMessageDOMap.get(issueVO.getCreatedBy());
        UserMessageDTO updaterUserDO = userMessageDOMap.get(issueDetailDTO.getLastUpdatedBy());
        if (!ObjectUtils.isEmpty(assigneeUserDO)) {
            issueVO.setAssigneeLoginName(assigneeUserDO.getLoginName());
            issueVO.setAssigneeRealName(assigneeUserDO.getRealName());
            issueVO.setAssigneeName(assigneeUserDO.getName());
            issueVO.setAssigneeImageUrl(assigneeUserDO.getImageUrl());
        }
        if (!ObjectUtils.isEmpty(reporterUserDO)) {
            issueVO.setReporterLoginName(reporterUserDO.getLoginName());
            issueVO.setReporterRealName(reporterUserDO.getRealName());
            issueVO.setReporterName(reporterUserDO.getName());
            issueVO.setReporterImageUrl(reporterUserDO.getImageUrl());
        }

        if (!ObjectUtils.isEmpty(createrUserDO)) {
            issueVO.setCreaterName(createrUserDO.getName());
            issueVO.setCreaterLoginName(createrUserDO.getLoginName());
            issueVO.setCreaterRealName(createrUserDO.getRealName());
            issueVO.setCreaterImageUrl(createrUserDO.getImageUrl());
            issueVO.setCreaterEmail(createrUserDO.getEmail());
        }

        issueVO.setUpdater(updaterUserDO);
        // 添加主要负责人、测试负责人信息
        issueVO.setMainResponsible(userMessageDOMap.get(issueDetailDTO.getMainResponsibleId()));
        // 添加参与人信息
        if (!CollectionUtils.isEmpty(issueDetailDTO.getParticipantIds())) {
            List<UserMessageDTO> participants = new ArrayList<>();
            for (Long participantId : issueDetailDTO.getParticipantIds()) {
                UserMessageDTO userMessageDTO = userMessageDOMap.get(participantId);
                if (!ObjectUtils.isEmpty(userMessageDTO)) {
                    participants.add(userMessageDTO);
                }
            }
            issueVO.setParticipants(participants);
        }
    }

    /**
     * issueDO转换到IssueListFieldKVDTO
     *
     * @param issueDTOList issueDetailDO
     * @return IssueListFieldKVVO
     */

    public List<IssueListFieldKVVO> issueDoToIssueListFieldKVDTO(List<IssueDTO> issueDTOList, Map<Long, PriorityVO> priorityMap, Map<Long, StatusVO> statusMapDTOMap, Map<Long, IssueTypeVO> issueTypeDTOMap, Map<Long, Map<String, Object>> foundationCodeValue, Map<Long, List<WorkLogVO>> workLogVOMap) {
        List<IssueListFieldKVVO> issueListFieldKVDTOList = new ArrayList<>(issueDTOList.size());
        Set<Long> userIds = issueDTOList.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueDTO::getAssigneeId).collect(Collectors.toSet());
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getReporterId() != null && !Objects.equals(issue.getReporterId(), 0L)).map(IssueDTO::getReporterId).collect(Collectors.toSet()));
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getCreatedBy() != null && !Objects.equals(issue.getCreatedBy(), 0L)).map(IssueDTO::getCreatedBy).collect(Collectors.toSet()));
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getLastUpdatedBy() != null && !Objects.equals(issue.getLastUpdatedBy(), 0L)).map(IssueDTO::getLastUpdatedBy).collect(Collectors.toSet()));
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getMainResponsibleId() != null && !Objects.equals(issue.getMainResponsibleId(), 0L)).map(IssueDTO::getMainResponsibleId).collect(Collectors.toSet()));
        issueDTOList.forEach(v -> {
            if (CollectionUtils.isNotEmpty(v.getParticipantIds())) {
                userIds.addAll(v.getParticipantIds());
            }
        });
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(Lists.newArrayList(userIds), true);
        Map<String, String> envMap = lookupValueService.queryMapByTypeCode(FieldCode.ENVIRONMENT);
        issueDTOList.forEach(issueDO -> {
            UserMessageDTO assigneeUserDO = usersMap.get(issueDO.getAssigneeId());
            UserMessageDTO reporterUserDO = usersMap.get(issueDO.getReporterId());
            IssueListFieldKVVO issueListFieldKVVO = toTarget(issueDO, IssueListFieldKVVO.class);
            if (!ObjectUtils.isEmpty(assigneeUserDO)) {
                issueListFieldKVVO.setAssigneeName(assigneeUserDO.getName());
                issueListFieldKVVO.setAssigneeLoginName(assigneeUserDO.getLoginName());
                issueListFieldKVVO.setAssigneeRealName(assigneeUserDO.getRealName());
                issueListFieldKVVO.setAssigneeImageUrl(assigneeUserDO.getImageUrl());
            }
            if (!ObjectUtils.isEmpty(reporterUserDO)) {
                issueListFieldKVVO.setReporterName(reporterUserDO.getName());
                issueListFieldKVVO.setReporterLoginName(reporterUserDO.getLoginName());
                issueListFieldKVVO.setReporterRealName(reporterUserDO.getRealName());
                issueListFieldKVVO.setReporterImageUrl(reporterUserDO.getImageUrl());
            }

            issueListFieldKVVO.setPriorityVO(priorityMap.get(issueDO.getPriorityId()));
            if (!ObjectUtils.isEmpty(issueTypeDTOMap)) {
                issueListFieldKVVO.setIssueTypeVO(issueTypeDTOMap.get(issueDO.getIssueTypeId()));
            }
            issueListFieldKVVO.setStatusVO(statusMapDTOMap.get(issueDO.getStatusId()));
            List<VersionIssueRelVO> versionList = toTargetList(issueDO.getVersionIssueRelDTOS(), VersionIssueRelVO.class);
            issueListFieldKVVO.setVersionIssueRelVOS(versionList);
            if (!CollectionUtils.isEmpty(versionList)) {
                issueListFieldKVVO.setFixVersionIssueRelVOS(versionList.stream().filter(versionIssueRelDTO -> Objects.equals(ProductVersionService.VERSION_RELATION_TYPE_FIX,versionIssueRelDTO.getRelationType())).collect(Collectors.toList()));
                issueListFieldKVVO.setInfluenceVersionIssueRelVOS(versionList.stream().filter(versionIssueRelDTO -> Objects.equals(ProductVersionService.VERSION_RELATION_TYPE_INFLUENCE,versionIssueRelDTO.getRelationType())).collect(Collectors.toList()));
            }
            issueListFieldKVVO.setIssueComponentBriefVOS(toTargetList(issueDO.getIssueComponentBriefDTOS(), IssueComponentBriefVO.class));
            issueListFieldKVVO.setIssueSprintVOS(toTargetList(issueDO.getIssueSprintDTOS(), IssueSprintVO.class));
            issueListFieldKVVO.setLabelIssueRelVOS(toTargetList(issueDO.getLabelIssueRelDTOS(), LabelIssueRelVO.class));
            issueListFieldKVVO.setFoundationFieldValue(foundationCodeValue.get(issueDO.getIssueId()) != null ? foundationCodeValue.get(issueDO.getIssueId()) : new HashMap<>());
            setParentId(issueListFieldKVVO, issueDO);
            issueListFieldKVVO.setCreateUser(usersMap.get(issueDO.getCreatedBy()));
            issueListFieldKVVO.setUpdateUser(usersMap.get(issueDO.getLastUpdatedBy()));
            issueListFieldKVVO.setMainResponsibleUser(usersMap.get(issueDO.getMainResponsibleId()));
            issueListFieldKVVO.setEnvironmentName(envMap.get(issueDO.getEnvironment()));
            issueListFieldKVVO.setTags(issueDO.getTags());
            setSpentWorkTimeAndAllEstimateTime(workLogVOMap, issueListFieldKVVO);
            if (CollectionUtils.isNotEmpty(issueDO.getParticipantIds())) {
               List<UserMessageDTO> participants = new ArrayList<>();
                for (Long participantId : issueDO.getParticipantIds()) {
                    UserMessageDTO userMessageDTO = usersMap.get(participantId);
                    if (!ObjectUtils.isEmpty(userMessageDTO)) {
                        participants.add(userMessageDTO);
                    }
                }
                issueListFieldKVVO.setParticipants(participants);
            }
            issueListFieldKVDTOList.add(issueListFieldKVVO);
        });
        return issueListFieldKVDTOList;
    }

    /**
     * 设置预估时间和耗费时间
     *
     * @param workLogVOMap workLogVOMap
     * @param issueListFieldKVVO issueListFieldKVVO
     */
    private void setSpentWorkTimeAndAllEstimateTime(Map<Long, List<WorkLogVO>> workLogVOMap, IssueListFieldKVVO issueListFieldKVVO) {
        List<WorkLogVO> workLogVOList = workLogVOMap.get(issueListFieldKVVO.getIssueId());
        BigDecimal spentWorkTime = null;
        BigDecimal allEstimateTime;
        if (!CollectionUtils.isEmpty(workLogVOList)) {
            spentWorkTime = new BigDecimal(0);
            for (WorkLogVO workLogVO : workLogVOList){
                spentWorkTime = spentWorkTime.add(workLogVO.getWorkTime());
            }
            allEstimateTime = issueListFieldKVVO.getRemainingTime() == null ? spentWorkTime : spentWorkTime.add(issueListFieldKVVO.getRemainingTime());
        } else {
            allEstimateTime = issueListFieldKVVO.getRemainingTime();
        }
        issueListFieldKVVO.setSpentWorkTime(spentWorkTime);
        issueListFieldKVVO.setAllEstimateTime(allEstimateTime);
    }

    /**
     * issueDTO转换为IssueCountVO
     * @param issueList issueList
     * @param priority 在priority内的user优先排序
     * @return IssueCountVO
     */
    public List<IssueCompletedStatusVO> issueDTOToIssueCountVO(List<IssueOverviewVO> issueList, Set<Long> priority){
        Set<Long> userIdList = new HashSet<>();
        Set<Long> creatorIds =
                issueList
                        .stream()
                        .map(IssueOverviewVO::getCreatedBy)
                        .collect(Collectors.toSet());
        userIdList.addAll(priority);
        userIdList.addAll(creatorIds);
        Map<Long, UserMessageDTO> userMap = userService.queryUsersMap(new ArrayList<>(userIdList), true);
        // 设置提出list
        List<Map.Entry<Long, Integer>> createdlist = sortAndConvertCreated(issueList, priority, userMap);
        // 设置已解决Map
        Map<Long, Integer> assigneeMap = sortAndConvertAssignee(issueList, userMap);
        List<IssueCompletedStatusVO> result = createdlist.stream()
                .map(entry -> new IssueCompletedStatusVO(entry.getKey(), entry.getValue())).collect(Collectors.toList());
        result.addAll(priority.stream()
                .filter(userId -> !ObjectUtils.isEmpty(userMap.get(userId)))
                .filter(userId -> !createdlist.stream().map(Map.Entry::getKey).collect(Collectors.toSet())
                        .contains(userId)).map(IssueCompletedStatusVO::new)
                .collect(Collectors.toList()));
        // 设置同一工作人的已解决问题数，并移除掉
        for (IssueCompletedStatusVO issue : result) {
            if (assigneeMap.containsKey(issue.getUserId())){
                issue.setCompleted(assigneeMap.get(issue.getUserId()));
                assigneeMap.remove(issue.getUserId());
            }
            UserMessageDTO userMessageDTO = userMap.get(issue.getUserId());
            if (!ObjectUtils.isEmpty(userMessageDTO)) {
                issue.setWorker(userMessageDTO.getName());
                issue.setUserMessage(userMessageDTO);
            }
        }
        // 将剩余的人（即仅解决bug无创建bug的人）加入list
        List<IssueCompletedStatusVO> issueCompletedStatusVOS = assigneeMap.entrySet().stream()
                .map(entry -> {
                    IssueCompletedStatusVO issueCompletedStatusVO = new IssueCompletedStatusVO(entry.getKey(), entry.getValue());
                    UserMessageDTO userMessageDTO = userMap.get(entry);
                    if (!ObjectUtils.isEmpty(userMessageDTO)) {
                        issueCompletedStatusVO.setWorker(userMessageDTO.getName());
                        issueCompletedStatusVO.setUserMessage(userMessageDTO);
                    }
                    return issueCompletedStatusVO;
                }).collect(Collectors.toList());
        result.addAll(issueCompletedStatusVOS);
        return result;
    }

    /**
     * 过滤掉未完成的issue再进行排序
     * @param issueList 待排序list
     * @return 坐标点list
     */
    private Map<Long, Integer> sortAndConvertAssignee(List<IssueOverviewVO> issueList, Map<Long, UserMessageDTO> userMap) {
        return issueList.stream()
                .filter(issue -> BooleanUtils.isTrue(issue.getCompleted()) && Objects.nonNull(issue.getAssigneeId()))
                .collect(Collectors.groupingBy(IssueOverviewVO::getAssigneeId)).entrySet()
                .stream().sorted(Map.Entry.comparingByKey())
                .filter(entry -> !ObjectUtils.isEmpty(userMap.get(entry.getKey())))
                .map(entry -> new ImmutablePair<>(entry.getKey(), entry.getValue().size()))
                .collect(Collectors.toMap(ImmutablePair::getLeft, ImmutablePair::getRight));
    }


    /**
     * 创建人是经办人或报告人时，排在前面
     * @param issueList 待排序list
     * @param priority 优先set
     * @param userMap 用户map
     * @return 坐标点list
     */
    private List<Map.Entry<Long, Integer>> sortAndConvertCreated(List<IssueOverviewVO> issueList, Set<Long> priority, Map<Long, UserMessageDTO> userMap) {
        List<Map.Entry<Long, Integer>> list = new ArrayList<>(issueList.size());
        Map<Boolean, List<IssueOverviewVO>> group = issueList.stream().collect(Collectors.groupingBy(issue -> priority.contains(issue.getCreatedBy())));
        list.addAll(group.getOrDefault(Boolean.TRUE, Collections.emptyList())
                .stream().collect(Collectors.groupingBy(IssueOverviewVO::getCreatedBy)).entrySet()
                .stream().sorted(Map.Entry.comparingByKey())
                .filter(entry -> !ObjectUtils.isEmpty(userMap.get(entry.getKey())))
                .map(entry -> new ImmutablePair<>(entry.getKey(), entry.getValue().size()))
                .collect(Collectors.toList()));
        list.addAll(group.getOrDefault(Boolean.FALSE, Collections.emptyList())
                .stream().collect(Collectors.groupingBy(IssueOverviewVO::getCreatedBy)).entrySet()
                .stream().sorted(Map.Entry.comparingByKey())
                .filter(entry -> !ObjectUtils.isEmpty(userMap.get(entry.getKey())))
                .map(entry -> new ImmutablePair<>(entry.getKey(), entry.getValue().size()))
                .collect(Collectors.toList()));
        return list;
    }

    protected void setParentId(IssueListFieldKVVO issueListFieldKVVO, IssueDTO issue) {
        Long parentId = null;
        Long parentIssueId = issue.getParentIssueId();
        Long relateIssueId = issue.getRelateIssueId();
        if (!ObjectUtils.isEmpty(parentIssueId) && parentIssueId != 0) {
            parentId = parentIssueId;
        }
        if (!ObjectUtils.isEmpty(relateIssueId) && relateIssueId != 0) {
            parentId = relateIssueId;
        }
        issueListFieldKVVO.setParentId(parentId);
    }


    /**
     * issueDO转换到IssueListDTO
     *
     * @param issueDTOList issueDetailDO
     * @return IssueListVO
     */
    public List<IssueListVO> issueDoToIssueListDto(List<IssueDTO> issueDTOList, Map<Long, PriorityVO> priorityMap, Map<Long, StatusVO> statusMapDTOMap, Map<Long, IssueTypeVO> issueTypeDTOMap) {
        List<IssueListVO> issueListDTOList = new ArrayList<>(issueDTOList.size());
        Set<Long> userIds = issueDTOList.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueDTO::getAssigneeId).collect(Collectors.toSet());
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getReporterId() != null && !Objects.equals(issue.getReporterId(), 0L)).map(IssueDTO::getReporterId).collect(Collectors.toSet()));
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(Lists.newArrayList(userIds), true);
        issueDTOList.forEach(issueDO -> {
            UserMessageDTO assigneeUserDO = usersMap.get(issueDO.getAssigneeId());
            UserMessageDTO reporterUserDO = usersMap.get(issueDO.getReporterId());
            IssueListVO issueListVO = toTarget(issueDO, IssueListVO.class);
            if (!ObjectUtils.isEmpty(assigneeUserDO)) {
                issueListVO.setAssigneeName(assigneeUserDO.getName());
                issueListVO.setAssigneeLoginName(assigneeUserDO.getLoginName());
                issueListVO.setAssigneeRealName(assigneeUserDO.getRealName());
                issueListVO.setAssigneeImageUrl(assigneeUserDO.getImageUrl());
            }
            if (!ObjectUtils.isEmpty(reporterUserDO)) {
                issueListVO.setReporterName(reporterUserDO.getName());
                issueListVO.setReporterLoginName(reporterUserDO.getLoginName());
                issueListVO.setReporterRealName(reporterUserDO.getRealName());
                issueListVO.setReporterImageUrl(reporterUserDO.getImageUrl());
            }

            issueListVO.setPriorityVO(priorityMap.get(issueDO.getPriorityId()));
            issueListVO.setIssueTypeVO(issueTypeDTOMap.get(issueDO.getIssueTypeId()));
            issueListVO.setStatusVO(statusMapDTOMap.get(issueDO.getStatusId()));

            issueListVO.setVersionIssueRelVOS(toTargetList(issueDO.getVersionIssueRelDTOS(), VersionIssueRelVO.class));
            issueListVO.setIssueComponentBriefVOS(toTargetList(issueDO.getIssueComponentBriefDTOS(), IssueComponentBriefVO.class));
            issueListVO.setIssueSprintVOS(toTargetList(issueDO.getIssueSprintDTOS(), IssueSprintVO.class));
            issueListVO.setLabelIssueRelVOS(toTargetList(issueDO.getLabelIssueRelDTOS(), LabelIssueRelVO.class));
            issueListDTOList.add(issueListVO);
        });
        return issueListDTOList;
    }

    /**
     * issueDO转换到subIssueDTO
     *
     * @param issueDTOList issueDTOList
     * @return SubIssueDTO
     */
    public List<IssueSubListVO> issueDoToSubIssueDto(List<IssueDTO> issueDTOList, Map<Long, IssueTypeVO> issueTypeDTOMap, Map<Long, StatusVO> statusMapDTOMap, Map<Long, PriorityVO> priorityDTOMap) {
        if (CollectionUtils.isEmpty(issueDTOList)){
            return Collections.emptyList();
        }
        List<IssueSubListVO> subIssueVOList = new ArrayList<>(issueDTOList.size());
        List<Long> assigneeIds = issueDTOList.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueDTO::getAssigneeId).distinct().collect(Collectors.toList());
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
        issueDTOList.forEach(issueDO -> {
            UserMessageDTO userMessageDTO = usersMap.get(issueDO.getAssigneeId());
            String assigneeName = userMessageDTO != null ? userMessageDTO.getName() : null;
            String imageUrl = userMessageDTO != null ? userMessageDTO.getImageUrl() : null;
            String loginName = userMessageDTO != null ? userMessageDTO.getLoginName() : null;
            String realName = userMessageDTO != null ? userMessageDTO.getRealName() : null;
            IssueSubListVO subIssueDTO = new IssueSubListVO();
            BeanUtils.copyProperties(issueDO, subIssueDTO);
            subIssueDTO.setAssigneeName(assigneeName);
            subIssueDTO.setImageUrl(imageUrl);
            subIssueDTO.setLoginName(loginName);
            subIssueDTO.setRealName(realName);
            subIssueDTO.setPriorityVO(priorityDTOMap.get(issueDO.getPriorityId()));
            subIssueDTO.setIssueTypeVO(issueTypeDTOMap.get(issueDO.getIssueTypeId()));
            subIssueDTO.setStatusVO(statusMapDTOMap.get(issueDO.getStatusId()));
            subIssueVOList.add(subIssueDTO);
        });
        return subIssueVOList;
    }

    /**
     * issueDetailDO转换到IssueSubDTO
     *
     * @param issueDetailDTO issueDetailDTO
     * @return IssueSubVO
     */
    public IssueSubVO issueDetailDoToIssueSubDto(IssueDetailDTO issueDetailDTO) {
        Long projectId = issueDetailDTO.getProjectId();
        String applyType = issueDetailDTO.getApplyType();
        IssueSubVO issueSubVO = new IssueSubVO();
        BeanUtils.copyProperties(issueDetailDTO, issueSubVO);
        issueSubVO.setComponentIssueRelVOList(modelMapper.map(issueDetailDTO.getComponentIssueRelDTOList(), new TypeToken<List<ComponentIssueRelVO>>(){}.getType()));
        issueSubVO.setVersionIssueRelVOList(modelMapper.map(issueDetailDTO.getVersionIssueRelDTOList(), new TypeToken<List<VersionIssueRelVO>>(){}.getType()));
        issueSubVO.setActiveSprint(sprintNameAssembler.toTarget(issueDetailDTO.getActiveSprint(), SprintNameVO.class));
        issueSubVO.setCloseSprint(sprintNameAssembler.toTargetList(issueDetailDTO.getCloseSprint(), SprintNameVO.class));
        issueSubVO.setLabelIssueRelVOList(modelMapper.map(issueDetailDTO.getLabelIssueRelDTOList(), new TypeToken<List<LabelIssueRelVO>>(){}.getType()));
        issueSubVO.setIssueAttachmentVOList(modelMapper.map(issueDetailDTO.getIssueAttachmentDTOList(), new TypeToken<List<IssueAttachmentVO>>(){}.getType()));
        issueSubVO.setIssueCommentVOList(modelMapper.map(issueDetailDTO.getIssueCommentDTOList(), new TypeToken<List<IssueCommentVO>>(){}.getType()));
        List<Long> assigneeIdList = new ArrayList<>();
        assigneeIdList.add(issueDetailDTO.getAssigneeId());
        assigneeIdList.add(issueDetailDTO.getReporterId());
        assigneeIdList.add(issueDetailDTO.getCreatedBy());
        Boolean issueCommentCondition = issueSubVO.getIssueCommentVOList() != null && !issueSubVO.getIssueCommentVOList().isEmpty();
        if (Boolean.TRUE.equals(issueCommentCondition)) {
            assigneeIdList.addAll(issueSubVO.getIssueCommentVOList().stream().map(IssueCommentVO::getUserId).collect(Collectors.toList()));
        }
        Map<Long, UserMessageDTO> userMessageDOMap = userService.queryUsersMap(
                assigneeIdList.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList()), true);
        UserMessageDTO assigneeDTO = userMessageDOMap.get(issueSubVO.getAssigneeId());
        if (!ObjectUtils.isEmpty(assigneeDTO)) {
            issueSubVO.setAssigneeName(assigneeDTO.getName());
            issueSubVO.setAssigneeImageUrl(assigneeDTO.getImageUrl());
        }

        UserMessageDTO reporterDTO = userMessageDOMap.get(issueSubVO.getReporterId());
        if (!ObjectUtils.isEmpty(reporterDTO)) {
            issueSubVO.setReporterName(reporterDTO.getName());
            issueSubVO.setReporterImageUrl(reporterDTO.getImageUrl());
        }
        UserMessageDTO createrDTO = userMessageDOMap.get(issueSubVO.getCreatedBy());
        if (!ObjectUtils.isEmpty(createrDTO)) {
            issueSubVO.setCreaterEmail(createrDTO.getEmail());
            issueSubVO.setCreaterName(createrDTO.getName());
            issueSubVO.setCreaterImageUrl(createrDTO.getImageUrl());
        }
        if (Boolean.TRUE.equals(issueCommentCondition)) {
            Map<Long, IssueCommentVO> commentMap = new HashMap<>(issueSubVO.getIssueCommentVOList().size());
            for (int i = issueSubVO.getIssueCommentVOList().size() - 1; i >= 0; i--) {
                IssueCommentVO issueCommentVO = issueSubVO.getIssueCommentVOList().get(i);
                UserMessageDTO commentUser = userMessageDOMap.get(issueCommentVO.getUserId());
                if (!ObjectUtils.isEmpty(commentUser)) {
                    issueCommentVO.setUserName(commentUser.getName());
                    issueCommentVO.setUserImageUrl(commentUser.getImageUrl());
                    issueCommentVO.setUserRealName(commentUser.getRealName());
                    issueCommentVO.setUserLoginName(commentUser.getLoginName());
                }
                issueCommentVO.setReplySize(0);
                commentMap.put(issueCommentVO.getCommentId(), issueCommentVO);
                if (issueCommentVO.getParentId() != null
                        && issueCommentVO.getParentId() != 0L
                        && !ObjectUtils.isEmpty(commentMap.get(issueCommentVO.getParentId()))) {
                    //设置被回复人信息
                    IssueCommentVO parentComment = commentMap.get(issueCommentVO.getParentId());
                    parentComment.setReplySize(parentComment.getReplySize() + 1);
                    issueCommentVO.setReplyToUserId(parentComment.getUserId());
                    issueCommentVO.setReplyToUserName(parentComment.getUserName());
                    issueCommentVO.setReplyToUserLoginName(parentComment.getUserLoginName());
                    issueCommentVO.setReplyToUserRealName(parentComment.getUserRealName());
                    issueCommentVO.setReplyToUserImageUrl(parentComment.getUserImageUrl());
                }
            }
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, applyType);
        issueSubVO.setIssueTypeVO(issueTypeDTOMap.get(issueDetailDTO.getIssueTypeId()));
        return issueSubVO;
    }

    public IssueCreateVO issueDtoToIssueCreateDto(IssueDetailDTO issueDetailDTO, List<String> predefinedFieldNames) {
        IssueCreateVO issueCreateVO = new IssueCreateVO();
        BeanUtils.copyProperties(issueDetailDTO, issueCreateVO);
        issueCreateVO.setSprintId(null);
        issueCreateVO.setRemainingTime(null);
        if (predefinedFieldNames.contains("component")) {
            issueCreateVO.setComponentIssueRelVOList(copyComponentIssueRel(issueDetailDTO.getComponentIssueRelDTOList()));
        }
        boolean copyFixVersion = predefinedFieldNames.contains("fixVersion");
        boolean copyInfluenceVersion = predefinedFieldNames.contains("influenceVersion");
        if (copyFixVersion || copyInfluenceVersion) {
            issueCreateVO.setVersionIssueRelVOList(copyVersionIssueRel(issueDetailDTO.getVersionIssueRelDTOList(), copyFixVersion, copyInfluenceVersion));
        }
        if (predefinedFieldNames.contains("label")) {
            issueCreateVO.setLabelIssueRelVOList(copyLabelIssueRel(issueDetailDTO.getLabelIssueRelDTOList(), issueDetailDTO.getProjectId()));
        }
        if (predefinedFieldNames.contains("tag")) {
            issueCreateVO.setTags(issueDetailDTO.getTags());
        }
        if (agilePluginService != null) {
            issueCreateVO.setProgramId(issueDetailDTO.getProjectId());
            Map<Long, List<ProductVO>> productMap = agilePluginService.listProductMap(ConvertUtil.getOrganizationId(issueDetailDTO.getProjectId()), Arrays.asList(issueDetailDTO.getProjectId()), Arrays.asList(issueDetailDTO.getIssueId()));
            List<ProductVO> productVOList = productMap.get(issueDetailDTO.getIssueId());
            if (!ObjectUtils.isEmpty(productVOList)) {
                issueCreateVO.setProductIds(productVOList.stream().map(ProductVO::getId).collect(Collectors.toList()));
            }
        }
        return issueCreateVO;
    }

    public IssueSubCreateVO issueDtoToIssueSubCreateDto(IssueDetailDTO issueDetailDTO, List<String> predefinedFieldNames) {
        IssueSubCreateVO issueSubCreateVO = new IssueSubCreateVO();
        BeanUtils.copyProperties(issueDetailDTO, issueSubCreateVO);
        issueSubCreateVO.setSprintId(null);
        issueSubCreateVO.setRemainingTime(null);
        if (predefinedFieldNames.contains("component")) {
            issueSubCreateVO.setComponentIssueRelVOList(copyComponentIssueRel(issueDetailDTO.getComponentIssueRelDTOList()));
        }
        boolean copyFixVersion = predefinedFieldNames.contains("fixVersion");
        boolean copyInfluenceVersion = predefinedFieldNames.contains("influenceVersion");
        if (copyFixVersion || copyInfluenceVersion) {
            issueSubCreateVO.setVersionIssueRelVOList(copyVersionIssueRel(issueDetailDTO.getVersionIssueRelDTOList(), copyFixVersion, copyInfluenceVersion));
        }
        if (predefinedFieldNames.contains("label")) {
            issueSubCreateVO.setLabelIssueRelVOList(copyLabelIssueRel(issueDetailDTO.getLabelIssueRelDTOList(), issueDetailDTO.getProjectId()));
        }
        if (predefinedFieldNames.contains("tag")) {
            issueSubCreateVO.setTags(issueDetailDTO.getTags());
        }
        if (agilePluginService != null) {
            Map<Long, List<ProductVO>> productMap = agilePluginService.listProductMap(ConvertUtil.getOrganizationId(issueDetailDTO.getProjectId()), Arrays.asList(issueDetailDTO.getProjectId()), Arrays.asList(issueDetailDTO.getIssueId()));
            List<ProductVO> productVOList = productMap.get(issueDetailDTO.getIssueId());
            if (!ObjectUtils.isEmpty(productVOList)) {
                issueSubCreateVO.setProductIds(productVOList.stream().map(ProductVO::getId).collect(Collectors.toList()));
            }
        }
        return issueSubCreateVO;
    }

    private List<ComponentIssueRelVO> copyComponentIssueRel(List<ComponentIssueRelDTO> componentIssueRelDTOList) {
        List<ComponentIssueRelVO> componentIssueRelVOList = new ArrayList<>(componentIssueRelDTOList.size());
        componentIssueRelDTOList.forEach(componentIssueRelDO -> {
            ComponentIssueRelVO componentIssueRelVO = new ComponentIssueRelVO();
            BeanUtils.copyProperties(componentIssueRelDO, componentIssueRelVO);
            componentIssueRelVO.setIssueId(null);
            componentIssueRelVO.setObjectVersionNumber(null);
            componentIssueRelVOList.add(componentIssueRelVO);
        });
        return componentIssueRelVOList;
    }

    private List<LabelIssueRelVO> copyLabelIssueRel(List<LabelIssueRelDTO> labelIssueRelDTOList, Long projectId) {
        List<LabelIssueRelVO> labelIssueRelVOList = new ArrayList<>(labelIssueRelDTOList.size());
        labelIssueRelDTOList.forEach(labelIssueRelDO -> {
            LabelIssueRelVO labelIssueRelVO = new LabelIssueRelVO();
            BeanUtils.copyProperties(labelIssueRelDO, labelIssueRelVO);
            labelIssueRelVO.setIssueId(null);
            labelIssueRelVO.setLabelName(null);
            labelIssueRelVO.setObjectVersionNumber(null);
            labelIssueRelVO.setProjectId(projectId);
            labelIssueRelVOList.add(labelIssueRelVO);
        });
        return labelIssueRelVOList;
    }

    private List<VersionIssueRelVO> copyVersionIssueRel(List<VersionIssueRelDTO> versionIssueRelDTOList, boolean copyFixVersion, boolean copyInfluenceVersion) {
        List<VersionIssueRelDTO> fixVersionIssueRelDTOList = versionIssueRelDTOList.stream().filter(v -> ProductVersionService.VERSION_RELATION_TYPE_FIX.equals(v.getRelationType())).collect(Collectors.toList());
        List<VersionIssueRelDTO> influenceVersionIssueRelDTOList = versionIssueRelDTOList.stream().filter(v -> ProductVersionService.VERSION_RELATION_TYPE_INFLUENCE.equals(v.getRelationType())).collect(Collectors.toList());
        List<VersionIssueRelVO> versionIssueRelVOList = new ArrayList<>(versionIssueRelDTOList.size());
        if (copyFixVersion) {
            fixVersionIssueRelDTOList.forEach(versionIssueRelDO -> {
                VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
                BeanUtils.copyProperties(versionIssueRelDO, versionIssueRelVO);
                versionIssueRelVO.setIssueId(null);
                versionIssueRelVOList.add(versionIssueRelVO);
            });
        }
        if (copyInfluenceVersion) {
            influenceVersionIssueRelDTOList.forEach(versionIssueRelDO -> {
                VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
                BeanUtils.copyProperties(versionIssueRelDO, versionIssueRelVO);
                versionIssueRelVO.setIssueId(null);
                versionIssueRelVOList.add(versionIssueRelVO);
            });
        }
        return versionIssueRelVOList;
    }

    public IssueSubCreateVO issueDtoToSubIssueCreateDto(IssueDetailDTO subIssueDetailDTO, Long parentIssueId) {
        IssueSubCreateVO issueCreateDTO = new IssueSubCreateVO();
        BeanUtils.copyProperties(subIssueDetailDTO, issueCreateDTO);
        String subSummary = subIssueDetailDTO.getSummary();
        issueCreateDTO.setSummary(subSummary);
        issueCreateDTO.setSprintId(null);
        issueCreateDTO.setIssueNum(null);
        issueCreateDTO.setParentIssueId(parentIssueId);
        issueCreateDTO.setComponentIssueRelVOList(copyComponentIssueRel(subIssueDetailDTO.getComponentIssueRelDTOList()));
        issueCreateDTO.setVersionIssueRelVOList(copyVersionIssueRel(subIssueDetailDTO.getVersionIssueRelDTOList(), true, true));
        issueCreateDTO.setLabelIssueRelVOList(copyLabelIssueRel(subIssueDetailDTO.getLabelIssueRelDTOList(), subIssueDetailDTO.getProjectId()));
        return issueCreateDTO;
    }

    public List<IssueComponentDetailDTO> issueComponentDetailDoToDto(Long projectId, List<IssueComponentDetailInfoDTO> issueComponentDetailInfoDTOS) {
        List<IssueComponentDetailDTO> issueComponentDetailDTOS = new ArrayList<>(issueComponentDetailInfoDTOS.size());
        if (!issueComponentDetailInfoDTOS.isEmpty()) {
            List<Long> userIds = issueComponentDetailInfoDTOS.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueComponentDetailInfoDTO::getAssigneeId).collect(Collectors.toList());
            userIds.addAll(issueComponentDetailInfoDTOS.stream().filter(issue -> issue.getReporterId() != null && !Objects.equals(issue.getReporterId(), 0L)).
                    map(IssueComponentDetailInfoDTO::getReporterId).collect(Collectors.toList()));
            Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(userIds.stream().distinct().collect(Collectors.toList()), true);
            Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(ConvertUtil.getOrganizationId(projectId), projectId);
            Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
            Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
            issueComponentDetailInfoDTOS.parallelStream().forEachOrdered(issueDO -> {
                UserMessageDTO assigneeDTO = usersMap.get(issueDO.getAssigneeId());
                UserMessageDTO reporterDTO = usersMap.get(issueDO.getReporterId());
                IssueComponentDetailDTO issueComponentDetailDTO = new IssueComponentDetailDTO();
                BeanUtils.copyProperties(issueDO, issueComponentDetailDTO);
                if (!ObjectUtils.isEmpty(assigneeDTO)) {
                    issueComponentDetailDTO.setAssigneeName(assigneeDTO.getName());
                    issueComponentDetailDTO.setAssigneeLoginName(assigneeDTO.getLoginName());
                    issueComponentDetailDTO.setAssigneeRealName(assigneeDTO.getRealName());
                    issueComponentDetailDTO.setAssigneeImageUrl(assigneeDTO.getImageUrl());
                }
                if (!ObjectUtils.isEmpty(reporterDTO)) {
                    issueComponentDetailDTO.setReporterName(reporterDTO.getName());
                    issueComponentDetailDTO.setReporterLoginName(reporterDTO.getLoginName());
                    issueComponentDetailDTO.setReporterRealName(reporterDTO.getRealName());
                    issueComponentDetailDTO.setReporterImageUrl(reporterDTO.getImageUrl());
                }

                issueComponentDetailDTO.setIssueTypeVO(issueTypeDTOMap.get(issueDO.getIssueTypeId()));
                issueComponentDetailDTO.setStatusVO(statusMapDTOMap.get(issueDO.getStatusId()));
                issueComponentDetailDTO.setPriorityVO(priorityDTOMap.get(issueDO.getPriorityId()));
                issueComponentDetailDTO.setComponentIssueRelVOList(modelMapper.map(issueDO.getComponentIssueRelDTOList(), new TypeToken<List<ComponentIssueRelVO>>(){}.getType()));
                issueComponentDetailDTO.setVersionIssueRelVOList(modelMapper.map(issueDO.getVersionIssueRelDTOList(),new TypeToken<List<VersionIssueRelVO>>(){}.getType()));
                issueComponentDetailDTO.setLabelIssueRelVOList(modelMapper.map(issueDO.getLabelIssueRelDTOList(), new TypeToken<List<LabelIssueRelVO>>(){}.getType()));
                issueComponentDetailDTOS.add(issueComponentDetailDTO);
            });
        }
        return issueComponentDetailDTOS;
    }

    public List<IssueListTestVO> issueDoToIssueTestListDto(List<IssueDTO> issueDTOList, Map<Long, PriorityVO> priorityMap, Map<Long, StatusVO> statusMapDTOMap, Map<Long, IssueTypeVO> issueTypeDTOMap) {
        List<IssueListTestVO> issueListTestVOS = new ArrayList<>(issueDTOList.size());
        Set<Long> userIds = issueDTOList.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueDTO::getAssigneeId).collect(Collectors.toSet());
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(Lists.newArrayList(userIds), true);
        issueDTOList.forEach(issueDO -> {
            String assigneeName = usersMap.get(issueDO.getAssigneeId()) != null ? usersMap.get(issueDO.getAssigneeId()).getName() : null;
            String assigneeImageUrl = assigneeName != null ? usersMap.get(issueDO.getAssigneeId()).getImageUrl() : null;
            IssueListTestVO issueListTestVO = toTarget(issueDO, IssueListTestVO.class);
            issueListTestVO.setAssigneeName(assigneeName);
            issueListTestVO.setPriorityVO(priorityMap.get(issueDO.getPriorityId()));
            issueListTestVO.setIssueTypeVO(issueTypeDTOMap.get(issueDO.getIssueTypeId()));
            issueListTestVO.setStatusVO(statusMapDTOMap.get(issueDO.getStatusId()));
            issueListTestVO.setAssigneeImageUrl(assigneeImageUrl);
            issueListTestVOS.add(issueListTestVO);
        });
        return issueListTestVOS;
    }

    public List<IssueNumVO> issueNumDoToDto(List<IssueNumDTO> issueNumDTOList, Long projectId) {
        List<IssueNumVO> issueNumVOS = new ArrayList<>(issueNumDTOList.size());
        if (!issueNumDTOList.isEmpty()) {
            Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(ConvertUtil.getOrganizationId(projectId), projectId);
            issueNumDTOList.forEach(issueDO -> {
                IssueNumVO issueNumVO = new IssueNumVO();
                BeanUtils.copyProperties(issueDO, issueNumVO);
                issueNumVO.setIssueTypeVO(issueTypeDTOMap.get(issueDO.getIssueTypeId()));
                issueNumVOS.add(issueNumVO);
            });
        }
        return issueNumVOS;
    }

    public List<UnfinishedIssueVO> unfinishedIssueDoToDto(List<UnfinishedIssueDTO> unfinishedIssueDTOS, Long projectId) {
        List<UnfinishedIssueVO> unfinishedIssueVOS = new ArrayList<>(unfinishedIssueDTOS.size());
        if (!unfinishedIssueDTOS.isEmpty()) {
            Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
            Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
            Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
            unfinishedIssueDTOS.forEach(unfinishedIssueDTO -> {
                UnfinishedIssueVO unfinishedIssueVO = toTarget(unfinishedIssueDTO, UnfinishedIssueVO.class);
                unfinishedIssueVO.setIssueTypeVO(issueTypeDTOMap.get(unfinishedIssueDTO.getIssueTypeId()));
                unfinishedIssueVO.setStatusVO(statusMapDTOMap.get(unfinishedIssueDTO.getStatusId()));
                unfinishedIssueVO.setPriorityVO(priorityDTOMap.get(unfinishedIssueDTO.getPriorityId()));
                unfinishedIssueVOS.add(unfinishedIssueVO);
            });

        }
        return unfinishedIssueVOS;
    }

    public List<UndistributedIssueVO> undistributedIssueDOToDto(List<UndistributedIssueDTO> undistributedIssueDTOS, Long projectId) {
        List<UndistributedIssueVO> undistributedIssueVOS = new ArrayList<>(undistributedIssueDTOS.size());
        if (!undistributedIssueDTOS.isEmpty()) {
            Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
            Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
            Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
            undistributedIssueDTOS.forEach(undistributedIssueDTO -> {
                UndistributedIssueVO undistributedIssueVO = toTarget(undistributedIssueDTO, UndistributedIssueVO.class);
                undistributedIssueVO.setIssueTypeVO(issueTypeDTOMap.get(undistributedIssueDTO.getIssueTypeId()));
                undistributedIssueVO.setStatusVO(statusMapDTOMap.get(undistributedIssueDTO.getStatusId()));
                undistributedIssueVO.setPriorityVO(priorityDTOMap.get(undistributedIssueDTO.getPriorityId()));
                undistributedIssueVOS.add(undistributedIssueVO);
            });

        }
        return undistributedIssueVOS;
    }

    public  List<IssueLinkVO> issueDTOTOVO(Long projectId, List<IssueDTO> issueDTOs){
        List<IssueLinkVO> issueLinkVOList = new ArrayList<>(issueDTOs.size());
        if (!issueDTOs.isEmpty()) {
            Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(ConvertUtil.getOrganizationId(projectId), projectId);
            Map<Long, StatusVO> statusMapDTOMap =
                    issueStatusMapper.listWithCompleted(projectId, ConvertUtil.getOrganizationId(projectId)).stream()
                            .collect(Collectors.toMap(StatusVO::getId, Function.identity()));
            Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
            List<Long> assigneeIds = issueDTOs.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueDTO::getAssigneeId).distinct().collect(Collectors.toList());
            Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
            issueDTOs.forEach(issueLinkDO -> {
                String assigneeName = usersMap.get(issueLinkDO.getAssigneeId()) != null ? usersMap.get(issueLinkDO.getAssigneeId()).getName() : null;
                String imageUrl = assigneeName != null ? usersMap.get(issueLinkDO.getAssigneeId()).getImageUrl() : null;
                io.choerodon.agile.api.vo.IssueLinkVO issueLinkVO = new io.choerodon.agile.api.vo.IssueLinkVO();
                BeanUtils.copyProperties(issueLinkDO, issueLinkVO);
                issueLinkVO.setIssueTypeVO(issueTypeDTOMap.get(issueLinkDO.getIssueTypeId()));
                issueLinkVO.setStatusVO(statusMapDTOMap.get(issueLinkDO.getStatusId()));
                issueLinkVO.setPriorityVO(priorityDTOMap.get(issueLinkDO.getPriorityId()));
                issueLinkVO.setAssigneeName(assigneeName);
                issueLinkVO.setImageUrl(imageUrl);
                issueLinkVOList.add(issueLinkVO);
            });
        }
        return  issueLinkVOList;
    }

    /**
     *  issueDTO转换SprintStatisticsVO
     * @param issueList issueList
     * @return SprintStatisticsVO
     */
    public SprintStatisticsVO issueDTOToSprintStatisticsVO(List<IssueOverviewVO> issueList) {
        SprintStatisticsVO sprintStatistics = new SprintStatisticsVO();
        Map<Boolean, List<IssueOverviewVO>> group = issueList.stream()
                .collect(Collectors.groupingBy(issue -> BooleanUtils.isTrue(issue.getCompleted())));
        sprintStatistics.setTotal(issueList.size());
        List<IssueOverviewVO> completedIssues = group.getOrDefault(Boolean.TRUE, Collections.emptyList());
        List<IssueOverviewVO> unCompletedIssues = group.getOrDefault(Boolean.FALSE, Collections.emptyList());
        List<IssueOverviewVO> todoIssues = unCompletedIssues.stream().filter(issue -> Objects.equals(StatusType.TODO, issue.getCategoryCode())).collect(Collectors.toList());
        List<IssueOverviewVO> unAssignIssues = issueList.stream().filter(issue -> Objects.isNull(issue.getAssigneeId())).collect(Collectors.toList());

        sprintStatistics.setCompletedCount(getIssueCountWithStatusIdsVO(completedIssues));
        sprintStatistics.setUncompletedCount(getIssueCountWithStatusIdsVO(unCompletedIssues));
        sprintStatistics.setTodoCount(getIssueCountWithStatusIdsVO(todoIssues));
        sprintStatistics.setUnassignCount(getIssueCountWithStatusIdsVO(unAssignIssues));
        return sprintStatistics;
    }

    private IssueCountWithStatusIdsVO getIssueCountWithStatusIdsVO(List<IssueOverviewVO> issues) {
        Set<Long> statusIds = issues.stream().map(IssueOverviewVO::getStatusId).collect(Collectors.toSet());
        Set<Long> issueTypeIds = issues.stream().map(IssueOverviewVO::getIssueTypeId).collect(Collectors.toSet());
        return new IssueCountWithStatusIdsVO(statusIds, issues.size(), issueTypeIds);
    }

    public List<Map.Entry<String, Integer>> convertBugEntry(List<ReportIssueConvertDTO> reportIssueConvertDTOList, DateFormat df, Predicate<ReportIssueConvertDTO> func){
        Map<Date, List<ReportIssueConvertDTO>> group = reportIssueConvertDTOList.stream()
                .filter(func::test).collect(Collectors.groupingBy(bug1 -> DateUtils.truncate(bug1.getDate(), Calendar.DAY_OF_MONTH)));
        // 去重
        for (Map.Entry<Date, List<ReportIssueConvertDTO>> entry : group.entrySet()) {
            List<ReportIssueConvertDTO> reportIssueConvertDTOS = group.get(entry.getKey());
            if (CollectionUtils.isNotEmpty(reportIssueConvertDTOS)) {
                List<ReportIssueConvertDTO> issueList = new ArrayList<>();
                List<ReportIssueConvertDTO> issueNullList = reportIssueConvertDTOS.stream().filter(v -> ObjectUtils.isEmpty(v.getIssueId())).collect(Collectors.toList());
                List<ReportIssueConvertDTO> issueNotNullList = reportIssueConvertDTOS.stream().filter(v -> !ObjectUtils.isEmpty(v.getIssueId())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(issueNullList)) {
                    issueList.addAll(issueNullList);
                }
                if (CollectionUtils.isNotEmpty(issueNotNullList)) {
                    issueNotNullList = issueNotNullList.stream().collect(collectingAndThen(
                            toCollection(() -> new TreeSet<>(comparing(ReportIssueConvertDTO::getIssueId))), ArrayList::new));
                    issueList.addAll(issueNotNullList);
                }
                group.put(entry.getKey(), issueList);
            }
        }
        return group.entrySet().stream().sorted(Map.Entry.comparingByKey())
                .map(entry -> new ImmutablePair<>(df.format(entry.getKey()),
                        entry.getValue().stream()
                                .map(v -> v.getNewValue().subtract(v.getOldValue()).intValue()).reduce(Integer::sum).orElse(0)))
                .collect(Collectors.toList());
    }

    public List<OneJobVO> issueToOneJob(SprintDTO sprint, List<IssueOverviewVO> issueList, List<WorkLogDTO> workLogList, List<DataLogDTO> resolutionLogList, List<DataLogDTO> assigneeLogList){
        // 生成迭代经办人，报告人list
        Set<Long> userSet = issueList.stream().map(IssueOverviewVO::getAssigneeId).collect(Collectors.toSet());
        userSet.addAll(issueList.stream().map(IssueOverviewVO::getReporterId).collect(Collectors.toSet()));
        if (CollectionUtils.isNotEmpty(workLogList)) {
            userSet.addAll(workLogList.stream().map(WorkLogDTO::getCreatedBy).collect(Collectors.toList()));
        }
        // 移除经办人为null的情况
        userSet.remove(null);
        userSet.remove(0L);
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATE);
        // 生成基准时间-用户轴
        Map<Date, Set<Long>> timeUserLine = generateTimeUserLine(sprint, userSet);
        Map<Long, UserMessageDTO> userMessageDOMap = userService.queryUsersMap(new ArrayList<>(userSet), true);
        // issue类型map
        Map<Long, IssueOverviewVO> issueTypeMap = issueList.stream().collect(Collectors.toMap(IssueOverviewVO::getIssueId,
                a -> a));
        // 每日每人bug提出数量
        Map<Date, Map<Long, List<IssueOverviewVO>>> dateOneBugMap = groupByList(
                issueList.stream().filter(issue -> IssueTypeCode.isBug(issue.getTypeCode())).collect(Collectors.toList()),
                IssueOverviewVO::getCreationDate, IssueOverviewVO::getCreatedBy,
                issue -> issue.setCreationDate(DateUtils.truncate(issue.getCreationDate(),
                        Calendar.DAY_OF_MONTH)));
        // 每日每人工时
        Map<Date, Map<Long, List<WorkLogDTO>>> dateOneWorkMap = groupByList(workLogList,
                WorkLogDTO::getStartDate, WorkLogDTO::getCreatedBy,
                issue -> issue.setStartDate(DateUtils.truncate(issue.getStartDate(),
                        Calendar.DAY_OF_MONTH)));
        // 每日issue最后经办人
        Map<Date, Map<Long, DataLogDTO>> assigneeMap = getAssigneeMap(assigneeLogList);
        // 计算任务，故事，解决bug
        Map<Date, List<DataLogDTO>> creationMap = getcreationMap(resolutionLogList, assigneeMap, issueTypeMap);
        Map<Date, OneJobVO> oneJobMap = timeUserLine.entrySet().stream().map(entry -> new ImmutablePair<>(entry.getKey()
                , dataLogListToOneJob(entry.getKey(), entry.getValue(),
                creationMap, issueTypeMap, dateOneBugMap, dateOneWorkMap, userMessageDOMap)))
                .collect(Collectors.toMap(ImmutablePair::getLeft, ImmutablePair::getRight));
        return oneJobMap.entrySet().stream().sorted(Map.Entry.comparingByKey()).map(entry -> {
            List<JobVO> jobList = entry.getValue().getJobList();
            entry.getValue().setTotal(new JobVO(jobList));
            entry.getValue().setWorkDate(df.format(entry.getValue().getWorkDateSource()));
            return entry.getValue();
        }).collect(Collectors.toList());
    }

    /**
     * 生成基准轴
     * @param sprint sprint
     * @param userSet userSet
     * @return 基准轴map
     */
    private Map<Date, Set<Long>> generateTimeUserLine(SprintDTO sprint, Set<Long> userSet) {
        Map<Date, Set<Long>> timeUserLine = new HashMap<>();
        // 生成迭代的开始时间与结束时间
        Date startDate = DateUtils.truncate(sprint.getStartDate(), Calendar.DAY_OF_MONTH);
        Date endDate = DateUtils.truncate(sprint.getActualEndDate(), Calendar.DAY_OF_MONTH);
        // 渲染基础时间轴，包含从迭代开始到目前的所有日期, 迭代所有涉及到的经办人和报告人
        if (startDate.after(endDate)){
            throw new CommonException(BaseConstants.ErrorCode.DATA_INVALID);
        }

        int days = DateUtil.differentDays(startDate, endDate);
        for (int i= 0; i <= days; i++){
            timeUserLine.put(startDate, new HashSet<>(userSet));
            startDate = DateUtils.addDays(startDate, 1);
        }
        return timeUserLine;
    }

    private Map<Date, List<DataLogDTO>> getcreationMap(List<DataLogDTO> dataLogList,
                                                       Map<Date, Map<Long, DataLogDTO>> assigneeMap,
                                                       Map<Long, IssueOverviewVO> issueTypeMap) {
        return dataLogList.stream()
                .peek(log -> log.setCreationDate(DateUtils.truncate(log.getCreationDate(), Calendar.DAY_OF_MONTH)))
                // 按照日志的创建日期分组
                .collect(Collectors.groupingBy(DataLogDTO::getCreationDate))
                .entrySet().stream()
                // 按照issueId去重，取logId最大值,即当天的最后解决记录
                .map(entry -> new ImmutablePair<>(entry.getKey(), entry.getValue().stream()
                        .collect(Collectors.groupingBy(DataLogDTO::getIssueId)).values()
                        .stream().map(list -> list.stream()
                                .max(Comparator.comparingLong(DataLogDTO::getLogId))
                                .map(log -> {
                                    DataLogDTO assignee = assigneeMap.getOrDefault(entry.getKey(), Collections.emptyMap())
                                            .getOrDefault(log.getIssueId(), new DataLogDTO());
                                    if (Objects.isNull(assignee.getNewValue())){
                                        log.setCreatedBy(issueTypeMap.get(log.getIssueId()).getAssigneeId());
                                    }else {
                                        log.setCreatedBy(Long.parseLong(assignee.getNewValue()));
                                    }
                                    return log;
                                })
                                .orElse(null)).filter(Objects::nonNull).collect(Collectors.toList())))
                .collect(Collectors.toMap(ImmutablePair::getLeft, ImmutablePair::getRight));
    }

    /**
     * 返回值 Map<日期, Map<issueId, assigneeId>>
     * @param assigneeLogList 经办人日志
     * @return Map
     */
    private Map<Date, Map<Long, DataLogDTO>> getAssigneeMap(List<DataLogDTO> assigneeLogList) {
        return assigneeLogList.stream()
                .peek(log -> log.setCreationDate(DateUtils.truncate(log.getCreationDate(), Calendar.DAY_OF_MONTH)))
                // 按照日志的创建日期分组
                .collect(Collectors.groupingBy(DataLogDTO::getCreationDate))
                .entrySet().stream()
                // 按照issueId去重，取logId最大值,即当天的经办人记录，
                // 如果无经办人则说明issue从创建就没更换过经办人，此时复制issueId, 直接取issue上的人。
                .map(entry -> new ImmutablePair<>(entry.getKey(), entry.getValue().stream()
                        .collect(Collectors.groupingBy(DataLogDTO::getIssueId)).entrySet()
                        .stream().map(e1 -> new ImmutablePair<>(e1.getKey(), e1.getValue().stream()
                                .max(Comparator.comparingLong(DataLogDTO::getLogId))
                                .orElseGet(() -> {
                                    DataLogDTO log = new DataLogDTO();
                                    log.setIssueId(e1.getKey());
                                    return log;
                                })
                        )).collect(Collectors.toMap(ImmutablePair::getLeft, ImmutablePair::getRight))))
                .collect(Collectors.toMap(ImmutablePair::getLeft, ImmutablePair::getRight));
    }

    private <T, K1, K2> Map<K1, Map<K2, List<T>>> groupByList(List<T> list,
                                                              Function<T, K1> k1,
                                                              Function<T, K2> k2,
                                                              Consumer<? super T> action){
        return list.stream().peek(action)
                .collect(Collectors.groupingBy(k1)).entrySet().stream()
                .map(entry -> new ImmutablePair<>(entry.getKey(), entry.getValue().stream()
                        .collect(Collectors.groupingBy(k2))))
                .collect(Collectors.toMap(ImmutablePair::getLeft, ImmutablePair::getRight));
    }

    private OneJobVO dataLogListToOneJob(Date workDate, Set<Long> userSet, Map<Date, List<DataLogDTO>> creationMap,
                                         Map<Long, IssueOverviewVO> issueTypeMap,
                                         Map<Date, Map<Long, List<IssueOverviewVO>>> dateOneBugMap,
                                         Map<Date, Map<Long, List<WorkLogDTO>>> dateOneWorkMap,
                                         Map<Long, UserMessageDTO> userMessageDOMap){
        // 根据日期取日志集合然后按照创建人分组
        Map<Long, List<DataLogDTO>> creationGroup = creationMap.getOrDefault(workDate, Collections.emptyList())
                .stream().filter(v -> !ObjectUtils.isEmpty(v.getCreatedBy()))
                .collect(Collectors.groupingBy(DataLogDTO::getCreatedBy));
        List<JobVO> jobList = new ArrayList<>(creationGroup.size());
        List<Long> userIds = new ArrayList(userSet);
        Collections.sort(userIds);
        for (Long userId : userIds) {
            // 取当前用户对应的解决issue集合，将集合按照issueType分组
            Map<String, List<DataLogDTO>> typeMap =
                    creationGroup.getOrDefault(userId, Collections.emptyList())
                            .stream().collect(Collectors.groupingBy(log -> issueTypeMap.get(log.getIssueId()).getTypeCode()));
            UserMessageDTO userMessageDTO = userMessageDOMap.get(userId);
            if (ObjectUtils.isEmpty(userMessageDTO)){
                continue;
            }
            JobVO job = new JobVO();
            job.setWorker(userMessageDTO.getRealName());
            job.setTaskCount(typeMap.getOrDefault(IssueTypeCode.TASK.value(), Collections.emptyList()).size()
                    + typeMap.getOrDefault(IssueTypeCode.SUB_TASK.value(), Collections.emptyList()).size());
            job.setStoryCount(typeMap.getOrDefault(IssueTypeCode.STORY.value(), Collections.emptyList()).size());
            job.setStoryPointCount(typeMap.getOrDefault(IssueTypeCode.STORY.value(), Collections.emptyList())
                    .stream().map(points -> issueTypeMap.get(points.getIssueId()).getStoryPoints())
                    .filter(Objects::nonNull).reduce(BigDecimal::add).orElse(BigDecimal.ZERO));
            job.setBugFixCount(typeMap.getOrDefault(IssueTypeCode.BUG.value(), Collections.emptyList()).size());
            job.setBugCreatedCount(dateOneBugMap.getOrDefault(workDate, Collections.emptyMap())
                    .getOrDefault(userId, Collections.emptyList()).size());
            job.setWorkTime(dateOneWorkMap.getOrDefault(workDate, Collections.emptyMap())
                    .getOrDefault(userId, Collections.emptyList())
                    .stream().map(WorkLogDTO::getWorkTime).filter(Objects::nonNull)
                    .reduce(BigDecimal::add).orElse(BigDecimal.ZERO));
            jobList.add(job);
        }
        OneJobVO oneJob = new OneJobVO();
        oneJob.setJobList(jobList);
        oneJob.setWorkDateSource(workDate);
        return oneJob;
    }
}
