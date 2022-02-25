package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssuePredecessorDTO;
import io.choerodon.agile.infra.dto.IssuePredecessorTreeClosureDTO;
import io.choerodon.agile.infra.dto.LookupValueDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.LookupType;
import io.choerodon.agile.infra.enums.PredecessorType;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.SearchVOUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2021-11-10
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssuePredecessorServiceImpl implements IssuePredecessorService {

    @Autowired
    private LookupValueMapper lookupValueMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssuePredecessorTreeClosureMapper issuePredecessorTreeClosureMapper;
    @Autowired
    private IssuePredecessorMapper issuePredecessorMapper;
    @Autowired
    private IssueService issueService;
    @Autowired
    private IssueAssembler issueAssembler;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private IssueTypeService issueTypeService;

    private static final List<String> ISSUE_TYPE_CODES =
            Arrays.asList(
                    IssueTypeCode.STORY.value(),
                    IssueTypeCode.BUG.value(),
                    IssueTypeCode.TASK.value(),
                    IssueTypeCode.SUB_TASK.value());

    private static final List<String> WATERFALL_ISSUE_TYPE_CODES =
            Arrays.asList(
                    IssueTypeCode.STAGE.value(),
                    IssueTypeCode.MILESTONE.value(),
                    IssueTypeCode.ACTIVITY.value());

    @Override
    public List<LookupValueDTO> queryPredecessorTypes(Long projectId) {
        LookupValueDTO example = new LookupValueDTO();
        example.setTypeCode(LookupType.PREDECESSOR_TYPE);
        return lookupValueMapper.select(example);
    }

    @Override
    public void updatePredecessors(Long projectId,
                                   List<IssuePredecessorVO> issuePredecessors,
                                   Long currentIssueId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<String, List<Long>> predecessorMap = new LinkedHashMap<>();
        validateIssuePredecessors(issuePredecessors, predecessorMap, currentIssueId);
        Set<Long> issueIds = validateIssueExisted(projectId, currentIssueId, predecessorMap);
        addSelfClosureIfNotExisted(issueIds, projectId, organizationId);
        //移除自身节点，返回所有直接父级
        issueIds.remove(currentIssueId);
        Set<Long> inputPredecessorIds = new HashSet<>(issueIds);
        Set<Long> existedPredecessorIds = queryExistedPredecessorIds(organizationId, projectId, currentIssueId);
        Set<Long> predecessorIds = new HashSet<>(inputPredecessorIds);
        predecessorIds.addAll(existedPredecessorIds);
        if (predecessorIds.isEmpty()) {
            return;
        }
        Set<Long> addPredecessorIds = new HashSet<>();
        Set<Long> deletePredecessorIds = new HashSet<>();
        Set<Long> intersection =
                processAddAndDeleteIds(addPredecessorIds, deletePredecessorIds, inputPredecessorIds, existedPredecessorIds);
        Set<Long> finalExistedPredecessorIds = new HashSet<>(intersection);
        finalExistedPredecessorIds.addAll(addPredecessorIds);
        List<IssuePredecessorTreeClosureDTO> ancestors =
                issuePredecessorTreeClosureMapper.selectByDescendantIds(organizationId, projectId, predecessorIds);
        //key直接父级，value为该路径下所有祖先
        Map<Long, Set<Long>> ancestorMap = new HashMap<>();
        ancestors.forEach(ancestor -> {
            Long descendantId = ancestor.getDescendantId();
            Set<Long> ancestorIds = ancestorMap.computeIfAbsent(descendantId, x -> new HashSet<>());
            ancestorIds.add(ancestor.getAncestorId());
        });
        List<IssuePredecessorTreeClosureDTO> descendants =
                issuePredecessorTreeClosureMapper.selectByAncestorIds(organizationId, projectId, new HashSet<>(Arrays.asList(currentIssueId)));
        Set<Long> descendantIds =
                descendants.stream().map(IssuePredecessorTreeClosureDTO::getDescendantId).collect(Collectors.toSet());
        addTreeNodes(projectId, organizationId, addPredecessorIds, ancestorMap, descendants, descendantIds);
        deleteTreeNodes(projectId, organizationId, descendants, ancestorMap, deletePredecessorIds, finalExistedPredecessorIds);
        insertIssPredecessor(organizationId, projectId, issuePredecessors, currentIssueId);
    }

    @Override
    public void addSelfNode(Long projectId, Long issueId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        IssuePredecessorTreeClosureDTO dto =
                buildIssuePredecessorTreeClosure(organizationId, projectId, issueId, issueId);
        batchInsertIfNotExisted(organizationId, projectId, new HashSet<>(Arrays.asList(dto)));
    }

    @Override
    public void deleteNode(Long projectId, Long issueId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        IssuePredecessorTreeClosureDTO example = new IssuePredecessorTreeClosureDTO();
        example.setProjectId(projectId);
        example.setOrganizationId(organizationId);
        example.setAncestorId(issueId);
        issuePredecessorTreeClosureMapper.delete(example);
        example.setAncestorId(null);
        example.setDescendantId(issueId);
        issuePredecessorTreeClosureMapper.delete(example);
        issuePredecessorMapper.deleteByIssueId(organizationId, projectId, issueId);
    }

    @Override
    public Page<IssueListFieldKVVO> pagedQueryEnabledIssues(Long organizationId,
                                                            Long projectId,
                                                            SearchVO searchVO,
                                                            PageRequest pageRequest,
                                                            Long currentIssueId) {
        SearchVOUtil.setTypeCodes(searchVO, getPredecessorIssueTypes(projectId));
        SearchVOUtil.setSearchArgs(searchVO, "tree", false);
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        searchVO.setWaterfallProject(ProjectCategory.isWaterfallProject(projectId));
        String excludeIssueIdsKey = "excludeIssueIds";
        Set<Long> excludeIssueIds = new HashSet<>();
        if (!ObjectUtils.isEmpty(otherArgs) && !ObjectUtils.isEmpty(otherArgs.get(excludeIssueIdsKey))) {
            List<String> excludeIssueIdsStr = (List<String>) otherArgs.get(excludeIssueIdsKey);
            excludeIssueIdsStr.forEach(x -> excludeIssueIds.add(Long.valueOf(x)));
            otherArgs.remove(excludeIssueIdsKey);
        }

        Set<Long> ignoredIssueIds = new HashSet<>();
        if (!Objects.isNull(currentIssueId)) {
            List<IssuePredecessorTreeClosureDTO> descendants =
                    issuePredecessorTreeClosureMapper.selectByAncestorIds(organizationId, projectId, new HashSet<>(Arrays.asList(currentIssueId)));
            Set<Long> predecessorIds =
                    issuePredecessorMapper.selectByIssueIds(new HashSet<>(Arrays.asList(projectId)), new HashSet<>(Arrays.asList(currentIssueId)))
                    .stream()
                    .map(IssuePredecessorDTO::getPredecessorId)
                    .collect(Collectors.toSet());
            ignoredIssueIds.addAll(descendants.stream().map(IssuePredecessorTreeClosureDTO::getDescendantId).collect(Collectors.toSet()));
            ignoredIssueIds.addAll(predecessorIds);
            ignoredIssueIds.add(currentIssueId);
        }
        String issueIdsKey = "issueIds";
        List<IssueListFieldKVVO> topIssues = new ArrayList<>();
        if (!ObjectUtils.isEmpty(otherArgs) && !ObjectUtils.isEmpty(otherArgs.get(issueIdsKey))) {
            List<String> issueIds = (List<String>) otherArgs.get(issueIdsKey);
            issueIds.forEach(str -> ignoredIssueIds.add(Long.valueOf(str)));
            PageRequest newPageRequest = new PageRequest(1, 0);
            topIssues.addAll(issueService.listIssueWithSub(projectId, searchVO, newPageRequest, organizationId));
            otherArgs.remove(issueIdsKey);
        }
        ignoredIssueIds.addAll(excludeIssueIds);
        SearchVOUtil.setOtherArgs(searchVO, "excludeIssueIds", ignoredIssueIds);
        Page<IssueListFieldKVVO> result = issueService.listIssueWithSub(projectId, searchVO, pageRequest, organizationId);
        topIssues.addAll(result.getContent());
        result.setContent(topIssues);
        return result;
    }

    @Override
    public List<IssuePredecessorVO> queryByIssueId(Long projectId, Long currentIssueId, boolean withInfo) {
        IssuePredecessorDTO dto = new IssuePredecessorDTO();
        dto.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        dto.setProjectId(projectId);
        dto.setIssueId(currentIssueId);
        List<IssuePredecessorDTO> dtoList = issuePredecessorMapper.select(dto);
        ModelMapper modelMapper = new ModelMapper();
        List<IssuePredecessorVO> result = modelMapper.map(dtoList, new TypeToken<List<IssuePredecessorVO>>() {
        }.getType());
        if (Boolean.TRUE.equals(withInfo) && !CollectionUtils.isEmpty(result)) {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            List<Long> issueIds = result.stream().map(IssuePredecessorVO::getPredecessorId).collect(Collectors.toList());
            // 类型、概要、编号、优先级、状态、经办人
            List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(issueIds, null, false, false);
            Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
            Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
            Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
            List<IssueListVO> issueList = issueAssembler.issueDoToIssueListDto(issueDTOList, priorityMap, statusMapDTOMap, issueTypeDTOMap);
            Map<Long, IssueListVO> issueMap = issueList.stream().collect(Collectors.toMap(IssueListVO::getIssueId, Function.identity()));
            result.forEach(vo -> vo.setPredecessorIssueVO(issueMap.get(vo.getPredecessorId())));
        }
        return result;
    }

    @Override
    public void deletePredecessor(Long projectId, Long predecessorId, Long currentIssueId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);

        List<IssuePredecessorTreeClosureDTO> currentIssueDescendants =
                issuePredecessorTreeClosureMapper.selectByAncestorIds(organizationId, projectId, new HashSet<>(Collections.singletonList(currentIssueId)));

        List<Long> samePredecessorIssueIds = issuePredecessorMapper.selectByPredecessorId(projectId, predecessorId);
        List<Long> ignoredDescendantIds = new ArrayList<>();
        samePredecessorIssueIds.remove(currentIssueId);
        if (!samePredecessorIssueIds.isEmpty()) {
            ignoredDescendantIds = issuePredecessorTreeClosureMapper.selectByAncestorIds(organizationId, projectId, new HashSet<>(samePredecessorIssueIds))
                    .stream().map(IssuePredecessorTreeClosureDTO::getDescendantId).collect(Collectors.toList());
        }
        // 过滤其他相同前置依赖的子级
        Set<IssuePredecessorTreeClosureDTO> deleteDescendants = new HashSet<>();
        for (IssuePredecessorTreeClosureDTO descendant : currentIssueDescendants) {
            if (!ignoredDescendantIds.contains(descendant.getDescendantId())) {
                deleteDescendants.add(descendant);
                descendant.setAncestorId(predecessorId);
            }
        }

        if (!deleteDescendants.isEmpty()) {
            issuePredecessorTreeClosureMapper.batchDelete(organizationId, projectId, deleteDescendants);
        }

        IssuePredecessorDTO delete = new IssuePredecessorDTO();
        delete.setProjectId(projectId);
        delete.setOrganizationId(organizationId);
        delete.setIssueId(currentIssueId);
        delete.setPredecessorId(predecessorId);
        issuePredecessorMapper.delete(delete);
    }

    private void deleteTreeNodes(Long projectId,
                                 Long organizationId,
                                 List<IssuePredecessorTreeClosureDTO> descendants,
                                 Map<Long, Set<Long>> ancestorMap,
                                 Set<Long> deletePredecessorIds,
                                 Set<Long> finalExistedPredecessorIds) {
        if (ObjectUtils.isEmpty(deletePredecessorIds)) {
            return;
        }
        Set<IssuePredecessorTreeClosureDTO> deleteNodes =
                buildDescendantByAncestor(descendants, ancestorMap, deletePredecessorIds);
        Set<IssuePredecessorTreeClosureDTO> ignoredNodes = new HashSet<>();
        if (!finalExistedPredecessorIds.isEmpty()) {
            ignoredNodes.addAll(buildDescendantByAncestor(descendants, ancestorMap, finalExistedPredecessorIds));
        }
        Set<IssuePredecessorTreeClosureDTO> filterDeleteNodes =
                deleteNodes.stream().filter(x -> !ignoredNodes.contains(x)).collect(Collectors.toSet());
        if (!filterDeleteNodes.isEmpty()) {
            issuePredecessorTreeClosureMapper.batchDelete(organizationId, projectId, filterDeleteNodes);
        }
    }

    private void addTreeNodes(Long projectId,
                              Long organizationId,
                              Set<Long> addPredecessorIds,
                              Map<Long, Set<Long>> ancestorMap,
                              List<IssuePredecessorTreeClosureDTO> descendants,
                              Set<Long> descendantIds) {
        if (!addPredecessorIds.isEmpty()) {
            validateCircularDependency(ancestorMap, descendantIds, addPredecessorIds);
            Set<IssuePredecessorTreeClosureDTO> insertSet = buildDescendantByAncestor(descendants, ancestorMap, addPredecessorIds);
            batchInsertIfNotExisted(organizationId, projectId, insertSet);
        }
    }

    private Set<Long> processAddAndDeleteIds(Set<Long> addPredecessorIds,
                                             Set<Long> deletePredecessorIds,
                                             Set<Long> inputPredecessorIds,
                                             Set<Long> existedPredecessorIds) {
        Set<Long> intersection = new HashSet<>();
        inputPredecessorIds.forEach(x -> {
            if (existedPredecessorIds.contains(x)) {
                intersection.add(x);
            }
        });
        inputPredecessorIds.forEach(x -> {
            if (!intersection.contains(x)) {
                addPredecessorIds.add(x);
            }
        });
        existedPredecessorIds.forEach(x -> {
            if (!intersection.contains(x)) {
                deletePredecessorIds.add(x);
            }
        });
        return intersection;
    }

    private Set<Long> queryExistedPredecessorIds(Long organizationId,
                                                 Long projectId,
                                                 Long currentIssueId) {
        IssuePredecessorDTO example = new IssuePredecessorDTO();
        example.setOrganizationId(organizationId);
        example.setProjectId(projectId);
        example.setIssueId(currentIssueId);
        return issuePredecessorMapper.select(example)
                .stream()
                .map(IssuePredecessorDTO::getPredecessorId)
                .collect(Collectors.toSet());
    }

    private void insertIssPredecessor(Long organizationId,
                                      Long projectId,
                                      List<IssuePredecessorVO> issuePredecessors,
                                      Long issueId) {
        IssuePredecessorDTO example = new IssuePredecessorDTO();
        example.setProjectId(projectId);
        example.setOrganizationId(organizationId);
        example.setIssueId(issueId);
        issuePredecessorMapper.delete(example);
        List<IssuePredecessorDTO> issuePredecessorList = new ArrayList<>();
        issuePredecessors.forEach(issuePredecessor -> {
            IssuePredecessorDTO dto = new IssuePredecessorDTO();
            dto.setIssueId(issuePredecessor.getIssueId());
            dto.setPredecessorId(issuePredecessor.getPredecessorId());
            dto.setPredecessorType(issuePredecessor.getPredecessorType().toLowerCase());
            dto.setProjectId(projectId);
            dto.setOrganizationId(organizationId);
            if (!issuePredecessorList.contains(dto)) {
                issuePredecessorList.add(dto);
            }
        });
        if (!issuePredecessorList.isEmpty()) {
            Long operator = DetailsHelper.getUserDetails().getUserId();
            issuePredecessorMapper.batchInsert(issuePredecessorList, operator);
        }
    }

    private void batchInsertIfNotExisted(Long organizationId,
                                         Long projectId,
                                         Set<IssuePredecessorTreeClosureDTO> treeNodeSet) {
        Set<IssuePredecessorTreeClosureDTO> existedSet =
                issuePredecessorTreeClosureMapper.selectInList(organizationId, projectId, treeNodeSet);
        Set<IssuePredecessorTreeClosureDTO> insertSet = new HashSet<>();
        treeNodeSet.forEach(node -> {
            if (!existedSet.contains(node)) {
                insertSet.add(node);
            }
        });
        if (!insertSet.isEmpty()) {
            Long operator = DetailsHelper.getUserDetails().getUserId();
            issuePredecessorTreeClosureMapper.batchInsert(insertSet, operator);
        }
    }

    private void validateCircularDependency(Map<Long, Set<Long>> ancestorMap,
                                            Set<Long> descendantIds,
                                            Set<Long> addPredecessorIds) {
        addPredecessorIds.forEach(predecessorId -> {
            Set<Long> ancestorIds = ancestorMap.get(predecessorId);
            ancestorIds.forEach(ancestorId -> {
                if (descendantIds.contains(ancestorId)) {
                    throw new CommonException("error.predecessor.circular.dependency");
                }
            });
        });
    }

    private Set<IssuePredecessorTreeClosureDTO> buildDescendantByAncestor(List<IssuePredecessorTreeClosureDTO> descendants,
                                                                          Map<Long, Set<Long>> ancestorMap,
                                                                          Set<Long> predecessorIds) {
        Set<IssuePredecessorTreeClosureDTO> result = new HashSet<>();
        descendants.forEach(descendant ->
                predecessorIds.forEach(predecessorId -> {
                    Set<Long> ancestorIds = ancestorMap.get(predecessorId);
                    ancestorIds.forEach(ancestorId -> {
                        IssuePredecessorTreeClosureDTO dto = new IssuePredecessorTreeClosureDTO();
                        BeanUtils.copyProperties(descendant, dto);
                        dto.setId(null);
                        dto.setAncestorId(ancestorId);
                        result.add(dto);
                    });
                })
        );
        return result;
    }

    private Set<Long> validateIssueExisted(Long projectId,
                                           Long currentIssueId,
                                           Map<String, List<Long>> predecessorMap) {
        Set<Long> issueIds = new HashSet<>();
        issueIds.add(currentIssueId);
        predecessorMap.forEach((type, ids) -> issueIds.addAll(ids));
        List<IssueDTO> issues = issueMapper.selectByIds(StringUtils.join(issueIds, ","));
        issues.forEach(issue -> {
            String typeCode = issue.getTypeCode();
            List<String> issueTypeCodes = getPredecessorIssueTypes(projectId);
            if (!issueTypeCodes.contains(typeCode)) {
                throw new CommonException("error.predecessor.illegal.issue.type");
            }
        });
        if (issueIds.size() != issues.size()) {
            Set<Long> existedIssueIds =
                    issues.stream().map(IssueDTO::getIssueId).collect(Collectors.toSet());
            Set<Long> notExistedIssueIds = new HashSet<>();
            issueIds.forEach(id -> {
                if (!existedIssueIds.contains(id)) {
                    notExistedIssueIds.add(id);
                }
            });
            throw new CommonException("error.predecessor.issue.not.existed");
        }
        return issueIds;
    }

    private void addSelfClosureIfNotExisted(Set<Long> issueIds,
                                            Long projectId,
                                            Long organizationId) {

        Set<IssuePredecessorTreeClosureDTO> treeNodes = new HashSet<>();
        issueIds.forEach(id ->
                treeNodes.add(buildIssuePredecessorTreeClosure(organizationId, projectId, id, id)));
        Set<IssuePredecessorTreeClosureDTO> existedTreeNodes =
                issuePredecessorTreeClosureMapper.selectInList(organizationId, projectId, treeNodes);
        Set<IssuePredecessorTreeClosureDTO> insertSet = new HashSet<>();
        treeNodes.forEach(node -> {
            if (!existedTreeNodes.contains(node)) {
                insertSet.add(node);
            }
        });
        if (!insertSet.isEmpty()) {
            Long operator = DetailsHelper.getUserDetails().getUserId();
            issuePredecessorTreeClosureMapper.batchInsert(insertSet, operator);
        }
    }

    private void validateIssuePredecessors(List<IssuePredecessorVO> issuePredecessors,
                                           Map<String, List<Long>> predecessorMap,
                                           Long currentIssueId) {
        List<Long> issueIds = new ArrayList<>();
        issuePredecessors.forEach(predecessor -> {
            Long issueId = predecessor.getIssueId();
            AssertUtilsForCommonException.notNull(issueId, "error.predecessor.issueId.null");
            issueIds.add(issueId);
            String predecessorType = predecessor.getPredecessorType();
            if (!PredecessorType.contains(predecessorType)) {
                throw new CommonException("error.illegal.predecessor.type");
            }
            Long predecessorId = predecessor.getPredecessorId();
            AssertUtilsForCommonException.notNull(predecessorId, "error.predecessor.id.null");
            predecessorType = predecessorType.toLowerCase();
            List<Long> predecessorIds = predecessorMap.computeIfAbsent(predecessorType, x -> new ArrayList<>());
            if (!predecessorIds.contains(predecessorId)) {
                predecessorIds.add(predecessorId);
            }
        });
        if (new HashSet<>(issueIds).size() > 1) {
            throw new CommonException("error.predecessor.add.only.allowed.one.issueId");
        }
        int totalCount = 0;
        Set<Long> predecessorIds = new HashSet<>();
        for (Map.Entry<String, List<Long>> entry : predecessorMap.entrySet()) {
            List<Long> list = entry.getValue();
            totalCount += list.size();
            predecessorIds.addAll(list);
        }
        if (predecessorIds.size() != totalCount) {
            throw new CommonException("error.predecessor.id.for.different.type");
        }
        if (predecessorIds.contains(currentIssueId)) {
            throw new CommonException("error.predecessor.circular.dependency");
        }
    }

    private IssuePredecessorTreeClosureDTO buildIssuePredecessorTreeClosure(Long organizationId,
                                                                            Long projectId,
                                                                            Long ancestorId,
                                                                            Long descendantId) {
        IssuePredecessorTreeClosureDTO dto = new IssuePredecessorTreeClosureDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setAncestorId(ancestorId);
        dto.setDescendantId(descendantId);
        return dto;
    }

    private List<String> getPredecessorIssueTypes(Long projectId) {
        if (ProjectCategory.isWaterfallProject(projectId)) {
            return WATERFALL_ISSUE_TYPE_CODES;
        }
        return ISSUE_TYPE_CODES;
    }
}
