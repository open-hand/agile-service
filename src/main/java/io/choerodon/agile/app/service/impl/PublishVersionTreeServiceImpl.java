package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.PublishVersionTreeService;
import io.choerodon.agile.infra.dto.PublishVersionDTO;
import io.choerodon.agile.infra.dto.PublishVersionTagRelDTO;
import io.choerodon.agile.infra.dto.PublishVersionTreeClosureDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.operator.DevopsClientOperator;
import io.choerodon.agile.infra.mapper.PublishVersionMapper;
import io.choerodon.agile.infra.mapper.PublishVersionTagRelMapper;
import io.choerodon.agile.infra.mapper.PublishVersionTreeClosureMapper;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2021-03-19
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PublishVersionTreeServiceImpl implements PublishVersionTreeService {

    @Autowired
    private PublishVersionTreeClosureMapper publishVersionTreeClosureMapper;
    @Autowired
    private PublishVersionMapper publishVersionMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PublishVersionTagRelMapper publishVersionTagRelMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private DevopsClientOperator devopsClientOperator;

    @Override
    public List<VersionTreeVO> tree(Set<Long> projectIds,
                                    Long organizationId,
                                    Set<Long> rootIds) {
        Set<PublishVersionTreeClosureDTO> versionTreeClosureSet =
                publishVersionTreeClosureMapper.selectDescendants(projectIds, organizationId, rootIds, null);
        Set<Long> childrenIds =
                versionTreeClosureSet
                        .stream()
                        .map(PublishVersionTreeClosureDTO::getDescendantId)
                        .collect(Collectors.toSet());
        Map<Long, PublishVersionDTO> publishVersionMap = new HashMap<>();
        if (!childrenIds.isEmpty()) {
            publishVersionMap.putAll(
                    publishVersionMapper.selectWithTag(childrenIds, projectIds, organizationId)
                            .stream()
                            .collect(Collectors.toMap(PublishVersionDTO::getId, Function.identity()))
            );
        }
        Map<Long, String> projectCodeMap =
                baseFeignClient.queryByIds(projectIds)
                        .getBody()
                        .stream()
                        .collect(Collectors.toMap(ProjectVO::getId, ProjectVO::getCode));
        Map<Long, Map<String, String>> appServiceCodeMap = queryAppServiceCodeMap(publishVersionMap, organizationId);
        List<VersionTreeVO> result = new ArrayList<>();
        rootIds.forEach(x -> result.add(toTree(versionTreeClosureSet, publishVersionMap, x, projectCodeMap, appServiceCodeMap)));
        return result;
    }

    private Map<Long, Map<String, String>> queryAppServiceCodeMap(Map<Long, PublishVersionDTO> publishVersionMap,
                                                                  Long organizationId) {
        Map<Long, Map<String, String>> result = new HashMap<>();
        if (!ObjectUtils.isEmpty(publishVersionMap)) {
            Set<AppServiceSimpleVO> appServiceSet = new HashSet<>();
            publishVersionMap.values()
                    .forEach(x -> {
                        Long projectId = x.getProjectId();
                        String appServiceCode = x.getServiceCode();
                        if (!ObjectUtils.isEmpty(projectId)
                                && !ObjectUtils.isEmpty(appServiceCode)) {
                            appServiceSet.add(buildAppServiceSimpleVO(projectId, appServiceCode));
                        }
                        if (!ObjectUtils.isEmpty(x.getTags())) {
                            x.getTags().forEach(y -> getAppServiceSimpleFromTag(appServiceSet, y));
                        }
                    });
            queryAppServiceCodeMapFromDevops(organizationId, result, appServiceSet);
        }
        return result;
    }

    private void getAppServiceSimpleFromTag(Set<AppServiceSimpleVO> appServiceSet, TagVO y) {
        Long tagProjectId = y.getProjectId();
        String tagAppServiceCode = y.getAppServiceCode();
        if (!ObjectUtils.isEmpty(tagProjectId)
                && !ObjectUtils.isEmpty(tagAppServiceCode)) {
            appServiceSet.add(buildAppServiceSimpleVO(tagProjectId, tagAppServiceCode));
        }
    }

    private void queryAppServiceCodeMapFromDevops(Long organizationId, Map<Long, Map<String, String>> result, Set<AppServiceSimpleVO> appServiceSet) {
        if (!appServiceSet.isEmpty()) {
            devopsClientOperator.listByProjectIdAndCode(organizationId, new ArrayList<>(appServiceSet))
                    .forEach(x -> {
                        Long projectId = x.getProjectId();
                        Map<String, String> codeNameMap = result.computeIfAbsent(projectId, y -> new HashMap<>());
                        String code = x.getAppServiceCode();
                        String name = x.getAppServiceName();
                        if (!StringUtils.isEmpty(code)
                                && !StringUtils.isEmpty(name)) {
                            codeNameMap.put(code, name);
                        }
                    });
        }
    }

    private AppServiceSimpleVO buildAppServiceSimpleVO(Long projectId, String appServiceCode) {
        AppServiceSimpleVO vo = new AppServiceSimpleVO();
        vo.setProjectId(projectId);
        vo.setAppServiceCode(appServiceCode);
        return vo;
    }


    @Override
    public void add(Long projectId,
                    Long organizationId,
                    VersionTreeVO versionTreeVO) {
        Long operator = DetailsHelper.getUserDetails().getUserId();
        Long parentId = versionTreeVO.getId();
        Set<Long> childrenIds = new HashSet<>();
        validateData(versionTreeVO, childrenIds);
        Set<Long> ancestorIds =
                publishVersionTreeClosureMapper.selectAncestors(projectId, organizationId, parentId).stream()
                        .map(PublishVersionTreeClosureDTO::getAncestorId)
                        .collect(Collectors.toSet());
        Set<PublishVersionTreeClosureDTO> descendants = new HashSet<>();
        if (!childrenIds.isEmpty()) {
            descendants.addAll(publishVersionTreeClosureMapper.selectDescendants(new HashSet<>(Arrays.asList(projectId)), organizationId, childrenIds, null));
            validateCircularDependency(descendants, ancestorIds);
        }
        Set<PublishVersionTreeClosureDTO> descendantSet = buildDescendantByAncestor(descendants, ancestorIds, parentId);
        insertListIfNotExisted(operator, descendantSet, projectId, organizationId);
    }

    @Override
    public void delete(Long projectId,
                       Long organizationId,
                       VersionTreeVO versionTreeVO) {
        Long parentId = versionTreeVO.getId();
        Set<Long> childrenIds = new HashSet<>();
        validateData(versionTreeVO, childrenIds);
        Set<Long> ancestorIds =
                publishVersionTreeClosureMapper.selectAncestors(projectId, organizationId, parentId).stream()
                        .map(PublishVersionTreeClosureDTO::getAncestorId)
                        .collect(Collectors.toSet());
        Set<PublishVersionTreeClosureDTO> descendants = new HashSet<>();
        if (!childrenIds.isEmpty()) {
            descendants.addAll(buildDeleteList(projectId, organizationId, childrenIds, ancestorIds, parentId));
        }
        if (!descendants.isEmpty()) {
            publishVersionTreeClosureMapper.batchDelete(descendants, projectId, organizationId);
        }
    }

    @Override
    public List<PublishVersionVO> availablePublishVersion(Long projectId, Long organizationId, Long rootId) {
        PublishVersionDTO publishVersionDTO = new PublishVersionDTO();
        publishVersionDTO.setProjectId(projectId);
        publishVersionDTO.setOrganizationId(organizationId);
        List<PublishVersionDTO> list = publishVersionMapper.select(publishVersionDTO);
        Set<Long> ancestorIds =
                publishVersionTreeClosureMapper.selectAncestors(projectId, organizationId, rootId)
                        .stream().map(PublishVersionTreeClosureDTO::getAncestorId).collect(Collectors.toSet());
        Set<Long> directDescendantIds =
                publishVersionTreeClosureMapper.selectDescendants(new HashSet<>(Arrays.asList(projectId)), organizationId, new HashSet<>(Arrays.asList(rootId)), null)
                        .stream()
                        .map(PublishVersionTreeClosureDTO::getDescendantId)
                        .collect(Collectors.toSet());
        Set<Long> ignoredIds = new HashSet<>(ancestorIds);
        ignoredIds.addAll(directDescendantIds);
        List<PublishVersionVO> result = new ArrayList<>();
        list.forEach(x -> {
            if (!ignoredIds.contains(x.getId())) {
                PublishVersionVO vo = new PublishVersionVO();
                BeanUtils.copyProperties(x, vo);
                result.add(vo);
            }
        });
        return result;
    }

    @Override
    public List<PublishVersionVO> directDescendants(Long projectId, Long organizationId, Long rootId) {
        PublishVersionTreeClosureDTO dto = new PublishVersionTreeClosureDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setAncestorId(rootId);
        dto.setDescendantParent(rootId);
        Set<Long> publishVersionIds =
                publishVersionTreeClosureMapper
                        .select(dto)
                        .stream()
                        .map(PublishVersionTreeClosureDTO::getDescendantId)
                        .collect(Collectors.toSet());
        if (publishVersionIds.isEmpty()) {
            return new ArrayList<>();
        }
        List<PublishVersionDTO> list = publishVersionMapper.selectByIds(StringUtils.join(publishVersionIds, ","));
        return modelMapper.map(list, new TypeToken<List<PublishVersionVO>>() {}.getType());
    }

    @Override
    public void addTag(Long projectId,
                       Long organizationId,
                       Long publishVersionId,
                       Set<TagVO> tags) {
        PublishVersionDTO publishVersion = publishVersionMapper.selectByPrimaryKey(publishVersionId);
        AssertUtilsForCommonException.notNull(publishVersion, "error.publish.version.not.existed.", publishVersionId);
        if (!ObjectUtils.isEmpty(tags)) {
            tags.forEach(x -> {
                PublishVersionTagRelDTO dto = buildPublishVersionTagRel(organizationId, publishVersionId, x);
                PublishVersionTagRelDTO example = new PublishVersionTagRelDTO();
                BeanUtils.copyProperties(dto, example);
                example.setTagAlias(null);
                List<PublishVersionTagRelDTO> result = publishVersionTagRelMapper.select(example);
                if (result.isEmpty()) {
                    publishVersionTagRelMapper.insert(dto);
                } else {
                    PublishVersionTagRelDTO updateObj = result.get(0);
                    updateObj.setTagAlias(dto.getTagAlias());
                    publishVersionTagRelMapper.updateByPrimaryKeySelective(updateObj);
                }
            });
        }
    }

    @Override
    public void deleteTag(Long projectId,
                          Long organizationId,
                          Long publishVersionId,
                          Set<TagVO> tags) {
        PublishVersionDTO publishVersion = publishVersionMapper.selectByPrimaryKey(publishVersionId);
        AssertUtilsForCommonException.notNull(publishVersion, "error.publish.version.not.existed.", publishVersionId);
        if (!ObjectUtils.isEmpty(tags)) {
            tags.forEach(x -> {
                PublishVersionTagRelDTO dto = buildPublishVersionTagRel(organizationId, publishVersionId, x);
                publishVersionTagRelMapper.delete(dto);
            });
        }
    }

    @Override
    public void updateTagAlias(Long projectId,
                               Long tagId,
                               Long publishVersionId,
                               Long objectVersionNumber,
                               String alias) {
        if(alias.length() > 16) {
            throw new CommonException("error.tag.alias.length.more.than.16");
        }
        if (StringUtils.isEmpty(alias)) {
            throw new CommonException("error.tag.alias.empty");
        }
        PublishVersionTagRelDTO dto = new PublishVersionTagRelDTO();
        dto.setId(tagId);
        dto.setPublishVersionId(publishVersionId);
        PublishVersionTagRelDTO result = publishVersionTagRelMapper.selectOne(dto);
        AssertUtilsForCommonException.notNull(result, "error.publish.version.tag.null");
        result.setObjectVersionNumber(objectVersionNumber);
        result.setTagAlias(alias);
        if (publishVersionTagRelMapper.updateByPrimaryKey(result) != 1) {
            throw new CommonException("error.update.publish.version.tag");
        }
    }

    private PublishVersionTagRelDTO buildPublishVersionTagRel(Long organizationId, Long publishVersionId, TagVO tag) {
        Long thisProjectId = tag.getProjectId();
        String appServiceCode = tag.getAppServiceCode();
        String tagName = tag.getTagName();
        AssertUtilsForCommonException.notNull(thisProjectId, "error.tag.projectId.null");
        AssertUtilsForCommonException.notEmpty(appServiceCode, "error.tag.appServiceCode.empty");
        AssertUtilsForCommonException.notEmpty(tagName, "error.tag.tagName.empty");
        PublishVersionTagRelDTO dto = new PublishVersionTagRelDTO();
        dto.setOrganizationId(organizationId);
        dto.setProjectId(thisProjectId);
        dto.setAppServiceCode(appServiceCode);
        dto.setTagName(tagName);
        dto.setPublishVersionId(publishVersionId);
        dto.setTagAlias(tag.getAlias());
        return dto;
    }

    private List<PublishVersionTreeClosureDTO> buildDeleteList(Long projectId,
                                                               Long organizationId,
                                                               Set<Long> childrenIds,
                                                               Set<Long> ancestorIds,
                                                               Long parentId) {
        Set<PublishVersionTreeClosureDTO> descendants =
                publishVersionTreeClosureMapper.selectDescendants(new HashSet<>(Arrays.asList(projectId)), organizationId, childrenIds, null);
        Set<PublishVersionTreeClosureDTO> descendantSet = buildDescendantByAncestor(descendants, ancestorIds, parentId);
        //祖先节点下所有不包含自己及自己祖先的节点
        Set<Long> nodeWithoutAncestorIds =
                publishVersionTreeClosureMapper
                        .selectDescendants(new HashSet<>(Arrays.asList(projectId)), organizationId, ancestorIds, null)
                        .stream()
                        .filter(x -> !ancestorIds.contains(x.getDescendantId()))
                        .map(PublishVersionTreeClosureDTO::getDescendantId)
                        .collect(Collectors.toSet());
        Set<PublishVersionTreeClosureDTO> input = new HashSet<>();
        descendantSet.forEach(x -> {
            PublishVersionTreeClosureDTO dto = new PublishVersionTreeClosureDTO();
            BeanUtils.copyProperties(x, dto);
            dto.setId(null);
            dto.setAncestorId(null);
            input.add(dto);
        });
        //校验这些后代数据是否存在于非ancestorIds的节点下
        Set<PublishVersionTreeClosureDTO> ignoredList =
                publishVersionTreeClosureMapper.selectAncestorsByIds(input, nodeWithoutAncestorIds, projectId, organizationId);
        List<PublishVersionTreeClosureDTO> result = new ArrayList<>();
        descendantSet.forEach(x -> {
            boolean notContains = true;
            for (PublishVersionTreeClosureDTO dto : ignoredList) {
                notContains =
                        notContains
                                && !(Objects.equals(x.getDescendantId(), dto.getDescendantId())
                                && Objects.equals(x.getDescendantParent(), dto.getDescendantParent()));
            }
            if (notContains) {
                result.add(x);
            }
        });
        return result;
    }

    private void insertListIfNotExisted(Long operator,
                                        Set<PublishVersionTreeClosureDTO> descendantSet,
                                        Long projectId,
                                        Long organizationId) {
        Set<PublishVersionTreeClosureDTO> existedList = publishVersionTreeClosureMapper.selectInList(descendantSet, projectId, organizationId);
        List<PublishVersionTreeClosureDTO> insertList = new ArrayList<>();
        descendantSet.forEach(x -> {
            if (!existedList.contains(x)) {
                insertList.add(x);
            }
        });
        if (!insertList.isEmpty()) {
            publishVersionTreeClosureMapper.batchInsert(new HashSet<>(insertList), operator);
        }
    }

    private Set<PublishVersionTreeClosureDTO> buildDescendantByAncestor(Set<PublishVersionTreeClosureDTO> descendants,
                                                                        Set<Long> ancestorIds,
                                                                        Long parentId) {
        Set<PublishVersionTreeClosureDTO> result = new HashSet<>();
        descendants.forEach(x ->
                ancestorIds.forEach(y -> {
                    Long descendantParent = x.getDescendantParent();
                    PublishVersionTreeClosureDTO dto = new PublishVersionTreeClosureDTO();
                    BeanUtils.copyProperties(x, dto);
                    dto.setId(null);
                    dto.setAncestorId(y);
                    if (Objects.equals(0L, descendantParent)) {
                        dto.setDescendantParent(parentId);
                    }
                    result.add(dto);
                }));
        return result;
    }

    private void validateCircularDependency(Set<PublishVersionTreeClosureDTO> descendants,
                                            Set<Long> ancestorIds) {
        Set<Long> descendantIds =
                descendants.stream().map(PublishVersionTreeClosureDTO::getDescendantId).collect(Collectors.toSet());
        ancestorIds.forEach(x -> {
            if (descendantIds.contains(x)) {
                throw new CommonException("error.version.tree.circular.dependency");
            }
        });
    }

    private void validateData(VersionTreeVO versionTreeVO,
                              Set<Long> childrenIds) {
        Long parentId = versionTreeVO.getId();
        PublishVersionDTO parent = publishVersionMapper.selectByPrimaryKey(parentId);
        if (parent == null) {
            throw new CommonException("error.parent.node.not.existed");
        }
        List<VersionTreeVO> childrenList = versionTreeVO.getChildren();
        if (ObjectUtils.isEmpty(childrenList)) {
            throw new CommonException("error.children.node.empty");
        }
        //去重
        Set<VersionTreeVO> children = new HashSet<>(childrenList);
        children.forEach(x -> {
            Long id = x.getId();
            if (id == null) {
                throw new CommonException("error.children.node.id.null");
            }
            childrenIds.add(id);
        });
        Set<Long> existedIds = new HashSet<>();
        if (!childrenIds.isEmpty()) {
            existedIds.addAll(publishVersionMapper
                    .selectByIds(StringUtils.join(childrenIds, ","))
                    .stream()
                    .map(PublishVersionDTO::getId)
                    .collect(Collectors.toSet()));
        }
        List<Long> illegalIds = new ArrayList<>();
        if (!Objects.equals(childrenIds.size(), existedIds.size())) {
            childrenIds.forEach(x -> {
                if (!existedIds.contains(x)) {
                    illegalIds.add(x);
                }
            });
        }
        if (!illegalIds.isEmpty()) {
            throw new CommonException("error.illegal.child.node.id." + StringUtils.join(illegalIds, ","));
        }
    }

    private VersionTreeVO toTree(Set<PublishVersionTreeClosureDTO> versionTreeClosureSet,
                                 Map<Long, PublishVersionDTO> publishVersionMap,
                                 Long rootId,
                                 Map<Long, String> projectCodeMap,
                                 Map<Long, Map<String, String>> appServiceCodeMap) {
        PublishVersionDTO publishVersionDTO = publishVersionMap.get(rootId);
        if (publishVersionDTO == null) {
            throw new CommonException("error.publish.version.not.existed." + rootId);
        }
        VersionTreeVO root = convertToVersionTree(publishVersionDTO, rootId, appServiceCodeMap);
        addPublishVersionTag(root, publishVersionDTO.getTags(), projectCodeMap, appServiceCodeMap);
        processChildNodes(root, versionTreeClosureSet, publishVersionMap, projectCodeMap, appServiceCodeMap);
        return root;
    }

    private void setAppServiceName(VersionTreeVO versionTreeVO,
                                   Map<Long, Map<String, String>> appServiceCodeMap) {
        Long projectId = versionTreeVO.getProjectId();
        String appServiceCode = versionTreeVO.getAppServiceCode();
        if (!ObjectUtils.isEmpty(projectId)
                && !ObjectUtils.isEmpty(appServiceCode)) {
            Map<String, String> codeNameMap = appServiceCodeMap.get(projectId);
            if (!ObjectUtils.isEmpty(codeNameMap)) {
                String appServiceName = codeNameMap.get(appServiceCode);
                if (!StringUtils.isEmpty(appServiceName)) {
                    versionTreeVO.setAppServiceName(appServiceName);
                }
            }
        }
    }

    private void addPublishVersionTag(VersionTreeVO root,
                                      List<TagVO> tags,
                                      Map<Long, String> projectCodeMap,
                                      Map<Long, Map<String, String>> appServiceCodeMap) {
        if (!ObjectUtils.isEmpty(tags)) {
            tags.forEach(x -> {
                StringBuilder builder = new StringBuilder();
                Long projectId = x.getProjectId();
                String appServiceCode = x.getAppServiceCode();
                String tagName = x.getTagName();
                builder
                        .append(projectCodeMap.get(projectId))
                        .append(":")
                        .append(appServiceCode)
                        .append(":")
                        .append(tagName);
                VersionTreeVO child = new VersionTreeVO();
                child.setType("tag");
                child.setName(builder.toString());
                child.setProjectId(projectId);
                child.setAppServiceCode(appServiceCode);
                child.setTagName(tagName);
                child.setId(x.getId());
                child.setTagAlias(x.getAlias());
                child.setObjectVersionNumber(x.getObjectVersionNumber());
                setAppServiceName(child, appServiceCodeMap);
                root.getChildren().add(child);
            });
        }
    }

    private void processChildNodes(VersionTreeVO root,
                                   Set<PublishVersionTreeClosureDTO> versionTreeClosureSet,
                                   Map<Long, PublishVersionDTO> publishVersionMap,
                                   Map<Long, String> projectCodeMap,
                                   Map<Long, Map<String, String>> appServiceCodeMap) {
        Long rootId = root.getId();
        List<VersionTreeVO> children = root.getChildren();
        versionTreeClosureSet.forEach(x -> {
            Long parentId = x.getDescendantParent();
            Long childId = x.getDescendantId();
            if (Objects.equals(rootId, parentId)) {
                PublishVersionDTO publishVersionDTO = publishVersionMap.get(childId);
                VersionTreeVO child = convertToVersionTree(publishVersionDTO, childId, appServiceCodeMap);
                addPublishVersionTag(child, publishVersionDTO.getTags(), projectCodeMap, appServiceCodeMap);
                children.add(child);
                processChildNodes(child, versionTreeClosureSet, publishVersionMap, projectCodeMap, appServiceCodeMap);
            }
        });
    }

    private VersionTreeVO convertToVersionTree(PublishVersionDTO publishVersionDTO,
                                               Long id,
                                               Map<Long, Map<String, String>> appServiceCodeMap) {
        VersionTreeVO versionTreeVO = new VersionTreeVO();
        versionTreeVO.setType("publish");
        versionTreeVO.setChildren(new ArrayList<>());
        versionTreeVO.setId(id);
        if (publishVersionDTO == null) {
            return versionTreeVO;
        }
        String name = publishVersionDTO.getArtifactId();
        versionTreeVO.setName(name);
        versionTreeVO.setVersion(publishVersionDTO.getVersion());
        versionTreeVO.setVersionAlias(publishVersionDTO.getVersionAlias());
        versionTreeVO.setGroupId(publishVersionDTO.getGroupId());
        versionTreeVO.setArtifactId(publishVersionDTO.getArtifactId());
        versionTreeVO.setAppService(publishVersionDTO.getAppService());
        versionTreeVO.setObjectVersionNumber(publishVersionDTO.getObjectVersionNumber());
        versionTreeVO.setAppServiceCode(publishVersionDTO.getServiceCode());
        versionTreeVO.setProjectId(publishVersionDTO.getProjectId());
        setAppServiceName(versionTreeVO, appServiceCodeMap);
        return versionTreeVO;
    }
}
