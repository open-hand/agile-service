package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.IssuePredecessorVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.service.IssuePredecessorService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.infra.dto.IssuePredecessorDTO;
import io.choerodon.agile.infra.dto.IssuePredecessorTreeClosureDTO;
import io.choerodon.agile.infra.dto.LookupValueDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.LookupType;
import io.choerodon.agile.infra.enums.PredecessorType;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.IssuePredecessorMapper;
import io.choerodon.agile.infra.mapper.IssuePredecessorTreeClosureMapper;
import io.choerodon.agile.infra.mapper.LookupValueMapper;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.SearchVOUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
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

    private static final List<String> ISSUE_TYPE_CODES =
            Arrays.asList(
                    IssueTypeCode.STORY.value(),
                    IssueTypeCode.BUG.value(),
                    IssueTypeCode.TASK.value(),
                    IssueTypeCode.SUB_TASK.value());

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
        Set<Long> issueIds = validateIssueExisted(currentIssueId, predecessorMap);
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
        processAddAndDeleteIds(addPredecessorIds, deletePredecessorIds, inputPredecessorIds, existedPredecessorIds);
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
        deleteTreeNodes(projectId, organizationId, descendants, ancestorMap, deletePredecessorIds);
        insertIssPredecessor(organizationId, projectId, issuePredecessors, currentIssueId);
    }

    @Override
    public void addSelfNode(Long projectId, Long issueId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        IssuePredecessorTreeClosureDTO dto =
                buildIssuePredecessorTreeClosure(organizationId, projectId, issueId, issueId, 0L);
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
        SearchVOUtil.setTypeCodes(searchVO, ISSUE_TYPE_CODES);
        SearchVOUtil.setSearchArgs(searchVO, "tree", false);
        List<IssuePredecessorTreeClosureDTO> ancestors =
                issuePredecessorTreeClosureMapper.selectByDescendantIds(organizationId, projectId, new HashSet<>(Arrays.asList(currentIssueId)));
        Set<Long> ignoredIssueIds =
                ancestors.stream().map(IssuePredecessorTreeClosureDTO::getAncestorId).collect(Collectors.toSet());
        if (!ignoredIssueIds.isEmpty()) {
            SearchVOUtil.setOtherArgs(searchVO, "excludeIssueIds", ignoredIssueIds);
        }
        return issueService.listIssueWithSub(projectId, searchVO, pageRequest, organizationId);
    }

    private void deleteTreeNodes(Long projectId,
                                 Long organizationId,
                                 List<IssuePredecessorTreeClosureDTO> descendants,
                                 Map<Long, Set<Long>> ancestorMap,
                                 Set<Long> deletePredecessorIds) {
        if (ObjectUtils.isEmpty(deletePredecessorIds)) {
            return;
        }
        Set<IssuePredecessorTreeClosureDTO> treeNodes = buildDescendantByAncestor(descendants, ancestorMap, deletePredecessorIds);
        Set<Long> ancestorIds = new HashSet<>();
        deletePredecessorIds.forEach(id -> ancestorIds.addAll(ancestorMap.get(id)));
        Set<IssuePredecessorTreeClosureDTO> ancestorSearchSet = new HashSet<>();
        ancestorIds.forEach(id -> {
            IssuePredecessorTreeClosureDTO dto = new IssuePredecessorTreeClosureDTO();
            dto.setAncestorId(id);
            ancestorSearchSet.add(dto);
        });
        //获取祖先所有的后代，不包含自己
        Set<IssuePredecessorTreeClosureDTO> descendantSet =
                issuePredecessorTreeClosureMapper.selectInList(organizationId, projectId, ancestorSearchSet);
        Set<Long> descendantIds =
                descendantSet
                        .stream()
                        .filter(x -> !ancestorIds.contains(x.getDescendantId()))
                        .map(IssuePredecessorTreeClosureDTO::getDescendantId)
                        .collect(Collectors.toSet());
        Set<IssuePredecessorTreeClosureDTO> nodeWithParentSearch = new HashSet<>();
        treeNodes.forEach(node -> {
            IssuePredecessorTreeClosureDTO dto = new IssuePredecessorTreeClosureDTO();
            dto.setDescendantId(node.getDescendantId());
            dto.setDescendantParent(node.getDescendantParent());
            nodeWithParentSearch.add(dto);
        });
        //查询要删除的节点在非祖先节点下是否存在，存在则要忽略
        Set<IssuePredecessorTreeClosureDTO> ignoredSearch = new HashSet<>();
        descendantIds.forEach(ancestorId ->
                nodeWithParentSearch.forEach(node -> {
                    Long descendantId = node.getDescendantId();
                    if (descendantId.equals(ancestorId)) {
                        return;
                    }
                    IssuePredecessorTreeClosureDTO dto = new IssuePredecessorTreeClosureDTO();
                    dto.setDescendantId(node.getDescendantId());
                    dto.setDescendantParent(node.getDescendantParent());
                    dto.setAncestorId(ancestorId);
                    ignoredSearch.add(dto);
                }));
        Set<IssuePredecessorTreeClosureDTO> ignoredSet = new HashSet<>();
        issuePredecessorTreeClosureMapper.selectInList(organizationId, projectId, ignoredSearch)
                .forEach(node -> {
                    IssuePredecessorTreeClosureDTO dto = new IssuePredecessorTreeClosureDTO();
                    dto.setDescendantParent(node.getDescendantParent());
                    dto.setDescendantId(node.getDescendantId());
                    ignoredSet.add(dto);
                });
        Set<IssuePredecessorTreeClosureDTO> deleteSet = new HashSet<>();
        treeNodes.forEach(node -> {
            boolean isDeleted = true;
            Long descendantId = node.getDescendantId();
            Long descendantParent = node.getDescendantParent();
            for (IssuePredecessorTreeClosureDTO ignoredNode : ignoredSet) {
                Long ignoredDescendantId = ignoredNode.getDescendantId();
                Long ignoredDescendantParent = ignoredNode.getDescendantParent();
                isDeleted = isDeleted
                        && !(Objects.equals(descendantId, ignoredDescendantId) && Objects.equals(descendantParent, ignoredDescendantParent));
            }
            if (isDeleted) {
                deleteSet.add(node);
            }
        });
        if (!deleteSet.isEmpty()) {
            issuePredecessorTreeClosureMapper.batchDelete(organizationId, projectId, deleteSet);
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

    private void processAddAndDeleteIds(Set<Long> addPredecessorIds,
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
            dto.setPredecessorType(issuePredecessor.getPredecessorType().toUpperCase());
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
        descendants.forEach(descendant -> {
            Long descendantParent = descendant.getDescendantParent();
            predecessorIds.forEach(predecessorId -> {
                Set<Long> ancestorIds = ancestorMap.get(predecessorId);
                ancestorIds.forEach(ancestorId -> {
                    IssuePredecessorTreeClosureDTO dto = new IssuePredecessorTreeClosureDTO();
                    BeanUtils.copyProperties(descendant, dto);
                    dto.setId(null);
                    dto.setAncestorId(ancestorId);
                    if (Objects.equals(0L, descendantParent)) {
                        dto.setDescendantParent(predecessorId);
                    }
                    result.add(dto);
                });
            });
        });
        return result;
    }

    private Set<Long> validateIssueExisted(Long currentIssueId,
                                           Map<String, List<Long>> predecessorMap) {
        Set<Long> issueIds = new HashSet<>();
        issueIds.add(currentIssueId);
        predecessorMap.forEach((type, ids) -> issueIds.addAll(ids));
        List<IssueDTO> issues = issueMapper.selectByIds(StringUtils.join(issueIds, ","));
        issues.forEach(issue -> {
            String typeCode = issue.getTypeCode();
            if (!ISSUE_TYPE_CODES.contains(typeCode)) {
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
                treeNodes.add(buildIssuePredecessorTreeClosure(organizationId, projectId, id, id, 0L)));
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
            predecessorType = predecessorType.toUpperCase();
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
                                                                            Long descendantId,
                                                                            Long descendantParent) {
        IssuePredecessorTreeClosureDTO dto = new IssuePredecessorTreeClosureDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setAncestorId(ancestorId);
        dto.setDescendantId(descendantId);
        dto.setDescendantParent(descendantParent);
        return dto;
    }
}
