package io.choerodon.agile.app.assembler;

import com.google.common.collect.Lists;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.LookupValueService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.enums.StatusType;
import io.choerodon.agile.infra.mapper.IssueStatusMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.dto.*;

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
import rx.Observable;

import java.text.DateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toCollection;

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
        issueVO.setSubIssueVOList(issueDoToSubIssueDto(issueDetailDTO.getSubIssueDTOList(), issueTypeDTOMap, statusMapDTOMap, priorityDTOMap));
        issueVO.setSubBugVOList(issueDoToSubIssueDto(issueDetailDTO.getSubBugDOList(), issueTypeDTOMap, statusMapDTOMap, priorityDTOMap));
        issueVO.setSameParentIssueVOList(issueDoToSubIssueDto(issueDetailDTO.getSameParentIssueDTOList(), issueTypeDTOMap, statusMapDTOMap, priorityDTOMap));
        issueVO.setSameParentBugVOList(issueDoToSubIssueDto(issueDetailDTO.getSameParentBugDOList(), issueTypeDTOMap, statusMapDTOMap, priorityDTOMap));
        issueVO.setPriorityVO(priorityDTOMap.get(issueVO.getPriorityId()));
        issueVO.setIssueTypeVO(issueTypeDTOMap.get(issueVO.getIssueTypeId()));
        issueVO.setStatusVO(statusMapDTOMap.get(issueVO.getStatusId()));
        List<Long> assigneeIdList = new ArrayList<>();
        assigneeIdList.add(issueDetailDTO.getAssigneeId());
        assigneeIdList.add(issueDetailDTO.getReporterId());
        assigneeIdList.add(issueDetailDTO.getCreatedBy());
        assigneeIdList.add(issueDetailDTO.getMainResponsibleId());
        assigneeIdList.add(issueDetailDTO.getLastUpdatedBy());
        Boolean issueCommentCondition = issueVO.getIssueCommentVOList() != null && !issueVO.getIssueCommentVOList().isEmpty();
        if (issueCommentCondition) {
            assigneeIdList.addAll(issueVO.getIssueCommentVOList().stream().map(IssueCommentVO::getUserId).collect(Collectors.toList()));
        }
        Map<Long, UserMessageDTO> userMessageDOMap = userService.queryUsersMap(
                assigneeIdList.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList()), true);
        UserMessageDTO assigneeUserDO = userMessageDOMap.get(issueVO.getAssigneeId());
        UserMessageDTO reporterUserDO = userMessageDOMap.get(issueVO.getReporterId());
        UserMessageDTO createrUserDO = userMessageDOMap.get(issueVO.getCreatedBy());
        UserMessageDTO updaterUserDO = userMessageDOMap.get(issueDetailDTO.getLastUpdatedBy());
        String assigneeName = assigneeUserDO != null ? assigneeUserDO.getName() : null;
        String assigneeLoginName = assigneeUserDO != null ? assigneeUserDO.getLoginName() : null;
        String assigneeRealName = assigneeUserDO != null ? assigneeUserDO.getRealName() : null;
        String reporterName = reporterUserDO != null ? reporterUserDO.getName() : null;
        String reporterLoginName = reporterUserDO != null ? reporterUserDO.getLoginName() : null;
        String reporterRealName = reporterUserDO != null ? reporterUserDO.getRealName() : null;
        issueVO.setAssigneeName(assigneeName);
        issueVO.setAssigneeImageUrl(assigneeName != null ? userMessageDOMap.get(issueVO.getAssigneeId()).getImageUrl() : null);
        issueVO.setReporterName(reporterName);
        issueVO.setReporterImageUrl(reporterName != null ? userMessageDOMap.get(issueVO.getReporterId()).getImageUrl() : null);
        issueVO.setCreaterName(createrUserDO != null ? createrUserDO.getName() : null);
        issueVO.setCreaterLoginName(createrUserDO != null ? createrUserDO.getLoginName() : null);
        issueVO.setCreaterRealName(createrUserDO != null ? createrUserDO.getRealName() : null);
        issueVO.setCreaterImageUrl(createrUserDO != null ? createrUserDO.getImageUrl() : null);
        issueVO.setCreaterEmail(createrUserDO != null ? createrUserDO.getEmail() : null);
        issueVO.setAssigneeLoginName(assigneeLoginName);
        issueVO.setAssigneeRealName(assigneeRealName);
        issueVO.setReporterLoginName(reporterLoginName);
        issueVO.setReporterRealName(reporterRealName);

        issueVO.setUpdater(updaterUserDO);
        if (issueCommentCondition) {
            Map<Long, Integer> parentSizeMap = new HashMap<>(issueVO.getIssueCommentVOList().size());
            Iterator<IssueCommentVO> iterator = issueVO.getIssueCommentVOList().iterator();
            while (iterator.hasNext()) {
                IssueCommentVO issueCommentVO = iterator.next();
                if (issueCommentVO.getParentId() != null && issueCommentVO.getParentId() != 0L) {
                    Integer size = parentSizeMap.get(issueCommentVO.getParentId()) != null ? parentSizeMap.get(issueCommentVO.getParentId()) : 0;
                    parentSizeMap.put(issueCommentVO.getParentId(), size + 1);
                    iterator.remove();
                } else {
                    UserMessageDTO commentUser = userMessageDOMap.get(issueCommentVO.getUserId());
                    issueCommentVO.setUserName(commentUser != null ? commentUser.getName() : null);
                    issueCommentVO.setUserImageUrl(commentUser != null ? commentUser.getImageUrl() : null);
                    issueCommentVO.setUserRealName(commentUser != null ? commentUser.getRealName() : null);
                    issueCommentVO.setUserLoginName(commentUser != null ? commentUser.getLoginName() : null);
                    issueCommentVO.setReplySize(parentSizeMap.get(issueCommentVO.getCommentId()) != null ? parentSizeMap.get(issueCommentVO.getCommentId()) : 0);
                }
            }
        }
        // 添加主要负责人、测试负责人信息
        issueVO.setMainResponsible(userMessageDOMap.get(issueDetailDTO.getMainResponsibleId()));
        return issueVO;
    }

    /**
     * issueDO转换到IssueListFieldKVDTO
     *
     * @param issueDTOList issueDetailDO
     * @return IssueListFieldKVVO
     */

    public List<IssueListFieldKVVO> issueDoToIssueListFieldKVDTO(List<IssueDTO> issueDTOList, Map<Long, PriorityVO> priorityMap, Map<Long, StatusVO> statusMapDTOMap, Map<Long, IssueTypeVO> issueTypeDTOMap, Map<Long, Map<String, Object>> foundationCodeValue) {
        List<IssueListFieldKVVO> issueListFieldKVDTOList = new ArrayList<>(issueDTOList.size());
        Set<Long> userIds = issueDTOList.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueDTO::getAssigneeId).collect(Collectors.toSet());
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getReporterId() != null && !Objects.equals(issue.getReporterId(), 0L)).map(IssueDTO::getReporterId).collect(Collectors.toSet()));
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getCreatedBy() != null && !Objects.equals(issue.getCreatedBy(), 0L)).map(IssueDTO::getCreatedBy).collect(Collectors.toSet()));
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getLastUpdatedBy() != null && !Objects.equals(issue.getLastUpdatedBy(), 0L)).map(IssueDTO::getLastUpdatedBy).collect(Collectors.toSet()));
        userIds.addAll(issueDTOList.stream().filter(issue -> issue.getMainResponsibleId() != null && !Objects.equals(issue.getMainResponsibleId(), 0L)).map(IssueDTO::getMainResponsibleId).collect(Collectors.toSet()));
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(Lists.newArrayList(userIds), true);
        Map<String, String> envMap = lookupValueService.queryMapByTypeCode(FieldCode.ENVIRONMENT);
        issueDTOList.forEach(issueDO -> {
            UserMessageDTO assigneeUserDO = usersMap.get(issueDO.getAssigneeId());
            UserMessageDTO reporterUserDO = usersMap.get(issueDO.getReporterId());
            String assigneeName = assigneeUserDO != null ? assigneeUserDO.getName() : null;
            String assigneeLoginName = assigneeUserDO != null ? assigneeUserDO.getLoginName() : null;
            String assigneeRealName = assigneeUserDO != null ? assigneeUserDO.getRealName() : null;
            String reporterName = reporterUserDO != null ? reporterUserDO.getName() : null;
            String reporterLoginName = reporterUserDO != null ? reporterUserDO.getLoginName() : null;
            String reporterRealName = reporterUserDO != null ? reporterUserDO.getRealName() : null;
            String assigneeImageUrl = assigneeUserDO != null ? assigneeUserDO.getImageUrl() : null;
            String reporterImageUrl = reporterUserDO != null ? reporterUserDO.getImageUrl() : null;
            IssueListFieldKVVO issueListFieldKVVO = toTarget(issueDO, IssueListFieldKVVO.class);
            issueListFieldKVVO.setAssigneeName(assigneeName);
            issueListFieldKVVO.setAssigneeLoginName(assigneeLoginName);
            issueListFieldKVVO.setAssigneeRealName(assigneeRealName);
            issueListFieldKVVO.setReporterName(reporterName);
            issueListFieldKVVO.setReporterLoginName(reporterLoginName);
            issueListFieldKVVO.setReporterRealName(reporterRealName);
            issueListFieldKVVO.setPriorityVO(priorityMap.get(issueDO.getPriorityId()));
            issueListFieldKVVO.setIssueTypeVO(issueTypeDTOMap.get(issueDO.getIssueTypeId()));
            issueListFieldKVVO.setStatusVO(statusMapDTOMap.get(issueDO.getStatusId()));
            issueListFieldKVVO.setAssigneeImageUrl(assigneeImageUrl);
            issueListFieldKVVO.setReporterImageUrl(reporterImageUrl);
            List<VersionIssueRelVO> versionList = toTargetList(issueDO.getVersionIssueRelDTOS(), VersionIssueRelVO.class);
            issueListFieldKVVO.setVersionIssueRelVOS(versionList);
            if (!CollectionUtils.isEmpty(versionList)) {
                issueListFieldKVVO.setFixVersionIssueRelVOS(versionList.stream().filter(versionIssueRelDTO -> Objects.equals("fix",versionIssueRelDTO.getRelationType())).collect(Collectors.toList()));
                issueListFieldKVVO.setInfluenceVersionIssueRelVOS(versionList.stream().filter(versionIssueRelDTO -> Objects.equals("influence",versionIssueRelDTO.getRelationType())).collect(Collectors.toList()));
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
            issueListFieldKVDTOList.add(issueListFieldKVVO);
        });
        return issueListFieldKVDTOList;
    }

    /**
     * issueDTO转换为IssueCountVO
     * @param issueList issueList
     * @param priority 在priority内的user优先排序
     * @return IssueCountVO
     */
    public List<IssueCompletedStatusVO> issueDTOToIssueCountVO(List<IssueOverviewVO> issueList, Set<Long> priority){
        Set<Long> userIdList = new HashSet<>();
        Observable.from(priority)
                .mergeWith(Observable.from(issueList.stream().map(IssueOverviewVO::getCreatedBy).collect(Collectors.toSet())))
                .toList().subscribe(userIdList::addAll);
        Map<Long, UserMessageDTO> userMap = userService.queryUsersMap(new ArrayList<>(userIdList), true);
        // 设置提出list
        List<Map.Entry<String, Integer>> createdlist = sortAndConvertCreated(issueList, priority, userMap);
        // 设置已解决Map
        Map<String, Integer> assigneeMap = sortAndConvertAssignee(issueList, userMap);
        List<IssueCompletedStatusVO> result = createdlist.stream()
                .map(entry -> new IssueCompletedStatusVO(entry.getKey(), entry.getValue())).collect(Collectors.toList());
        result.addAll(priority.stream()
                .filter(userId -> !ObjectUtils.isEmpty(userMap.get(userId)))
                .map(userId -> userMap.get(userId).getRealName())
                .filter(realName -> !createdlist.stream().map(Map.Entry::getKey).collect(Collectors.toSet())
                        .contains(realName)).map(IssueCompletedStatusVO::new)
                .collect(Collectors.toList()));
        // 设置同一工作人的已解决问题数，并移除掉
        for (IssueCompletedStatusVO issue : result) {
            if (assigneeMap.containsKey(issue.getWorker())){
                issue.setCompleted(assigneeMap.get(issue.getWorker()));
                assigneeMap.remove(issue.getWorker());
            }
        }
        // 将剩余的人（即仅解决bug无创建bug的人）加入list
        result.addAll(assigneeMap.entrySet().stream()
                .map(entry -> new IssueCompletedStatusVO(entry.getKey(), entry.getValue()))
                .collect(Collectors.toList()));
        return result;
    }

    /**
     * 过滤掉未完成的issue再进行排序
     * @param issueList 待排序list
     * @return 坐标点list
     */
    private Map<String, Integer> sortAndConvertAssignee(List<IssueOverviewVO> issueList, Map<Long, UserMessageDTO> userMap) {
        return issueList.stream()
                .filter(issue -> BooleanUtils.isTrue(issue.getCompleted()) && Objects.nonNull(issue.getAssigneeId()))
                .collect(Collectors.groupingBy(IssueOverviewVO::getAssigneeId)).entrySet()
                .stream().sorted(Map.Entry.comparingByKey())
                .filter(entry -> !ObjectUtils.isEmpty(userMap.get(entry.getKey())))
                .map(entry -> new ImmutablePair<>(userMap.get(entry.getKey()).getRealName(), entry.getValue().size()))
                .collect(Collectors.toMap(ImmutablePair::getLeft, ImmutablePair::getRight));
    }


    /**
     * 创建人是经办人或报告人时，排在前面
     * @param issueList 待排序list
     * @param priority 优先set
     * @param userMap 用户map
     * @return 坐标点list
     */
    private List<Map.Entry<String, Integer>> sortAndConvertCreated(List<IssueOverviewVO> issueList, Set<Long> priority, Map<Long, UserMessageDTO> userMap) {
        List<Map.Entry<String, Integer>> list = new ArrayList<>(issueList.size());
        Map<Boolean, List<IssueOverviewVO>> group = issueList.stream().collect(Collectors.groupingBy(issue -> priority.contains(issue.getCreatedBy())));
        list.addAll(group.getOrDefault(Boolean.TRUE, Collections.emptyList())
                .stream().collect(Collectors.groupingBy(IssueOverviewVO::getCreatedBy)).entrySet()
                .stream().sorted(Map.Entry.comparingByKey())
                .filter(entry -> !ObjectUtils.isEmpty(userMap.get(entry.getKey())))
                .map(entry -> new ImmutablePair<>(userMap.get(entry.getKey()).getRealName(), entry.getValue().size()))
                .collect(Collectors.toList()));
        list.addAll(group.getOrDefault(Boolean.FALSE, Collections.emptyList())
                .stream().collect(Collectors.groupingBy(IssueOverviewVO::getCreatedBy)).entrySet()
                .stream().sorted(Map.Entry.comparingByKey())
                .filter(entry -> !ObjectUtils.isEmpty(userMap.get(entry.getKey())))
                .map(entry -> new ImmutablePair<>(userMap.get(entry.getKey()).getRealName(), entry.getValue().size()))
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
            String assigneeName = assigneeUserDO != null ? assigneeUserDO.getName() : null;
            String assigneeLoginName = assigneeUserDO != null ? assigneeUserDO.getLoginName() : null;
            String assigneeRealName = assigneeUserDO != null ? assigneeUserDO.getRealName() : null;
            String reporterName = reporterUserDO != null ? reporterUserDO.getName() : null;
            String reporterLoginName = reporterUserDO != null ? reporterUserDO.getLoginName() : null;
            String reporterRealName = reporterUserDO != null ? reporterUserDO.getRealName() : null;
            String assigneeImageUrl = assigneeUserDO != null ? assigneeUserDO.getImageUrl() : null;
            String reporterImageUrl = reporterUserDO != null ? reporterUserDO.getImageUrl() : null;
            IssueListVO issueListVO = toTarget(issueDO, IssueListVO.class);
            issueListVO.setAssigneeName(assigneeName);
            issueListVO.setAssigneeLoginName(assigneeLoginName);
            issueListVO.setAssigneeRealName(assigneeRealName);
            issueListVO.setReporterName(reporterName);
            issueListVO.setReporterLoginName(reporterLoginName);
            issueListVO.setReporterRealName(reporterRealName);
            issueListVO.setPriorityVO(priorityMap.get(issueDO.getPriorityId()));
            issueListVO.setIssueTypeVO(issueTypeDTOMap.get(issueDO.getIssueTypeId()));
            issueListVO.setStatusVO(statusMapDTOMap.get(issueDO.getStatusId()));
            issueListVO.setAssigneeImageUrl(assigneeImageUrl);
            issueListVO.setReporterImageUrl(reporterImageUrl);
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
    protected List<IssueSubListVO> issueDoToSubIssueDto(List<IssueDTO> issueDTOList, Map<Long, IssueTypeVO> issueTypeDTOMap, Map<Long, StatusVO> statusMapDTOMap, Map<Long, PriorityVO> priorityDTOMap) {
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
        if (issueCommentCondition) {
            assigneeIdList.addAll(issueSubVO.getIssueCommentVOList().stream().map(IssueCommentVO::getUserId).collect(Collectors.toList()));
        }
        Map<Long, UserMessageDTO> userMessageDOMap = userService.queryUsersMap(
                assigneeIdList.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList()), true);
        String assigneeName = userMessageDOMap.get(issueSubVO.getAssigneeId()) != null ? userMessageDOMap.get(issueSubVO.getAssigneeId()).getName() : null;
        String reporterName = userMessageDOMap.get(issueSubVO.getReporterId()) != null ? userMessageDOMap.get(issueSubVO.getReporterId()).getName() : null;
        String createrName = userMessageDOMap.get(issueSubVO.getCreatedBy()) != null ? userMessageDOMap.get(issueSubVO.getCreatedBy()).getName() : null;
        issueSubVO.setCreaterEmail(userMessageDOMap.get(issueSubVO.getCreatedBy()) != null ? userMessageDOMap.get(issueSubVO.getCreatedBy()).getEmail() : null);
        issueSubVO.setAssigneeName(assigneeName);
        issueSubVO.setAssigneeImageUrl(assigneeName != null ? userMessageDOMap.get(issueSubVO.getAssigneeId()).getImageUrl() : null);
        issueSubVO.setReporterName(reporterName);
        issueSubVO.setReporterImageUrl(reporterName != null ? userMessageDOMap.get(issueSubVO.getReporterId()).getImageUrl() : null);
        issueSubVO.setCreaterName(createrName);
        issueSubVO.setCreaterImageUrl(createrName != null ? userMessageDOMap.get(issueSubVO.getCreatedBy()).getImageUrl() : null);
        if (issueCommentCondition) {
            Map<Long, IssueCommentVO> commentMap = new HashMap<>(issueSubVO.getIssueCommentVOList().size());
            for (int i = issueSubVO.getIssueCommentVOList().size() - 1; i >= 0; i--) {
                IssueCommentVO issueCommentVO = issueSubVO.getIssueCommentVOList().get(i);
                UserMessageDTO commentUser = userMessageDOMap.get(issueCommentVO.getUserId());
                issueCommentVO.setUserName(commentUser != null ? commentUser.getName() : null);
                issueCommentVO.setUserImageUrl(commentUser != null ? commentUser.getImageUrl() : null);
                issueCommentVO.setUserRealName(commentUser != null ? commentUser.getRealName() : null);
                issueCommentVO.setUserLoginName(commentUser != null ? commentUser.getLoginName() : null);
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
        return issueSubVO;
    }

    public IssueCreateVO issueDtoToIssueCreateDto(IssueDetailDTO issueDetailDTO) {
        IssueCreateVO issueCreateVO = new IssueCreateVO();
        BeanUtils.copyProperties(issueDetailDTO, issueCreateVO);
        issueCreateVO.setSprintId(null);
        issueCreateVO.setRemainingTime(null);
        issueCreateVO.setComponentIssueRelVOList(copyComponentIssueRel(issueDetailDTO.getComponentIssueRelDTOList()));
        issueCreateVO.setVersionIssueRelVOList(copyVersionIssueRel(issueDetailDTO.getVersionIssueRelDTOList()));
        issueCreateVO.setLabelIssueRelVOList(copyLabelIssueRel(issueDetailDTO.getLabelIssueRelDTOList(), issueDetailDTO.getProjectId()));
        return issueCreateVO;
    }

    public IssueSubCreateVO issueDtoToIssueSubCreateDto(IssueDetailDTO issueDetailDTO) {
        IssueSubCreateVO issueSubCreateVO = new IssueSubCreateVO();
        BeanUtils.copyProperties(issueDetailDTO, issueSubCreateVO);
        issueSubCreateVO.setSprintId(null);
        issueSubCreateVO.setRemainingTime(null);
        issueSubCreateVO.setComponentIssueRelVOList(copyComponentIssueRel(issueDetailDTO.getComponentIssueRelDTOList()));
        issueSubCreateVO.setVersionIssueRelVOList(copyVersionIssueRel(issueDetailDTO.getVersionIssueRelDTOList()));
        issueSubCreateVO.setLabelIssueRelVOList(copyLabelIssueRel(issueDetailDTO.getLabelIssueRelDTOList(), issueDetailDTO.getProjectId()));
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

    private List<VersionIssueRelVO> copyVersionIssueRel(List<VersionIssueRelDTO> versionIssueRelDTOList) {
        List<VersionIssueRelVO> versionIssueRelVOList = new ArrayList<>(versionIssueRelDTOList.size());
        versionIssueRelDTOList.forEach(versionIssueRelDO -> {
            VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
            BeanUtils.copyProperties(versionIssueRelDO, versionIssueRelVO);
            versionIssueRelVO.setIssueId(null);
            versionIssueRelVOList.add(versionIssueRelVO);
        });
        return versionIssueRelVOList;
    }

    public IssueSubCreateVO issueDtoToSubIssueCreateDto(IssueDetailDTO subIssueDetailDTO, Long parentIssueId) {
        IssueSubCreateVO issueCreateDTO = new IssueSubCreateVO();
        BeanUtils.copyProperties(subIssueDetailDTO, issueCreateDTO);
        String subSummary = "CLONE-" + subIssueDetailDTO.getSummary();
        issueCreateDTO.setSummary(subSummary);
        issueCreateDTO.setSprintId(null);
        issueCreateDTO.setIssueNum(null);
        issueCreateDTO.setParentIssueId(parentIssueId);
        issueCreateDTO.setComponentIssueRelVOList(copyComponentIssueRel(subIssueDetailDTO.getComponentIssueRelDTOList()));
        issueCreateDTO.setVersionIssueRelVOList(copyVersionIssueRel(subIssueDetailDTO.getVersionIssueRelDTOList()));
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
            Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.TEST);
            Map<Long, IssueTypeVO> issueTypeDTOMapAgile = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
            issueTypeDTOMap.putAll(issueTypeDTOMapAgile);
            Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
            Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
            issueComponentDetailInfoDTOS.parallelStream().forEachOrdered(issueDO -> {
                String assigneeName = usersMap.get(issueDO.getAssigneeId()) != null ? usersMap.get(issueDO.getAssigneeId()).getName() : null;
                String assigneeLoginName = usersMap.get(issueDO.getAssigneeId()) != null ? usersMap.get(issueDO.getAssigneeId()).getLoginName() : null;
                String assigneeRealName = usersMap.get(issueDO.getAssigneeId()) != null ? usersMap.get(issueDO.getAssigneeId()).getRealName() : null;
                String reporterName = usersMap.get(issueDO.getReporterId()) != null ? usersMap.get(issueDO.getReporterId()).getName() : null;
                String reporterLoginName = usersMap.get(issueDO.getReporterId()) != null ? usersMap.get(issueDO.getReporterId()).getLoginName() : null;
                String reporterRealName = usersMap.get(issueDO.getReporterId()) != null ? usersMap.get(issueDO.getReporterId()).getRealName() : null;
                String assigneeImageUrl = assigneeName != null ? usersMap.get(issueDO.getAssigneeId()).getImageUrl() : null;
                String reporterImageUrl = reporterName != null ? usersMap.get(issueDO.getReporterId()).getImageUrl() : null;
                IssueComponentDetailDTO issueComponentDetailDTO = new IssueComponentDetailDTO();
                BeanUtils.copyProperties(issueDO, issueComponentDetailDTO);
                issueComponentDetailDTO.setAssigneeName(assigneeName);
                issueComponentDetailDTO.setAssigneeLoginName(assigneeLoginName);
                issueComponentDetailDTO.setAssigneeRealName(assigneeRealName);
                issueComponentDetailDTO.setReporterName(reporterName);
                issueComponentDetailDTO.setReporterLoginName(reporterLoginName);
                issueComponentDetailDTO.setReporterRealName(reporterRealName);
                issueComponentDetailDTO.setAssigneeImageUrl(assigneeImageUrl);
                issueComponentDetailDTO.setReporterImageUrl(reporterImageUrl);
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
            Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
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
            Map<Long, IssueTypeVO> testIssueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.TEST);
            Map<Long, IssueTypeVO> agileIssueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
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
                if (issueLinkDO.getApplyType().equals(SchemeApplyType.TEST)) {
                    issueLinkVO.setIssueTypeVO(testIssueTypeDTOMap.get(issueLinkDO.getIssueTypeId()));
                } else {
                    issueLinkVO.setIssueTypeVO(agileIssueTypeDTOMap.get(issueLinkDO.getIssueTypeId()));
                }
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

        long unAssignCount =
                completedIssues
                        .stream()
                        .filter(issue -> Objects.isNull(issue.getAssigneeId()))
                        .count();
        unAssignCount +=
                unCompletedIssues
                        .stream()
                        .filter(issue -> Objects.isNull(issue.getAssigneeId()))
                        .count();
        sprintStatistics.setUnassignCount((int) unAssignCount);

        Set<Long> completedIssueStatusIds = completedIssues.stream().map(IssueOverviewVO::getStatusId).collect(Collectors.toSet());
        Set<Long> unCompletedIssueStatusIds = unCompletedIssues.stream().map(IssueOverviewVO::getStatusId).collect(Collectors.toSet());
        Set<Long> todoIssueStatusIds = todoIssues.stream().map(IssueOverviewVO::getStatusId).collect(Collectors.toSet());
        sprintStatistics.setCompletedCount(new IssueCountWithStatusIdsVO(completedIssueStatusIds, completedIssues.size()));
        sprintStatistics.setUncompletedCount(new IssueCountWithStatusIdsVO(unCompletedIssueStatusIds, unCompletedIssues.size()));
        sprintStatistics.setTodoCount(new IssueCountWithStatusIdsVO(todoIssueStatusIds, todoIssues.size()));
        return sprintStatistics;
    }

    public List<Map.Entry<String, Integer>> convertBugEntry(List<ReportIssueConvertDTO> reportIssueConvertDTOList, DateFormat df, Function<ReportIssueConvertDTO, Boolean> func){
        Map<Date, List<ReportIssueConvertDTO>> group = reportIssueConvertDTOList.stream()
                .filter(func::apply).collect(Collectors.groupingBy(bug1 -> DateUtils.truncate(bug1.getDate(), Calendar.DAY_OF_MONTH)));
        // 去重
        for (Date date : group.keySet()) {
            List<ReportIssueConvertDTO> reportIssueConvertDTOS = group.get(date);
            if (CollectionUtils.isNotEmpty(reportIssueConvertDTOList)) {
               group.put(date, reportIssueConvertDTOS.stream().collect(collectingAndThen(
                       toCollection(() -> new TreeSet<>(comparing(n->n.getIssueId()))),ArrayList::new)));
            }
        }
        return group.entrySet().stream().sorted(Map.Entry.comparingByKey())
                .map(entry -> new ImmutablePair<>(df.format(entry.getKey()),
                        entry.getValue().stream()
                                .map(v -> v.getNewValue().subtract(v.getOldValue()).intValue()).reduce(Integer::sum).orElse(0)))
                .collect(Collectors.toList());
    }
}
