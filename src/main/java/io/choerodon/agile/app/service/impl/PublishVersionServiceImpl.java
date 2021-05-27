package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.web.multipart.MultipartFile;
import org.xml.sax.SAXException;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;
import javax.xml.parsers.ParserConfigurationException;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.operator.DevopsClientOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author superlee
 * @since 2021-03-10
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PublishVersionServiceImpl implements PublishVersionService {

    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PublishVersionMapper publishVersionMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private DevopsClientOperator devopsClientOperator;
    @Autowired
    private PomService pomService;
    @Autowired
    private PublishVersionTreeClosureMapper publishVersionTreeClosureMapper;
    @Autowired
    private PublishVersionTreeService publishVersionTreeService;
    @Autowired
    private IssueService issueService;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private TagIssueRelMapper tagIssueRelMapper;
    @Autowired
    private TagCompareHistoryMapper tagCompareHistoryMapper;
    @Autowired
    private VersionTagHistoryService versionTagHistoryService;
    @Autowired
    private PublishVersionTagRelMapper publishVersionTagRelMapper;
    @Autowired
    private MessageClientC7n messageClientC7n;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private UserService userService;

    protected static final Logger logger = LoggerFactory.getLogger(PublishVersionServiceImpl.class);

    private static final String GROUP_ID_EMPTY_EXCEPTION = "error.publish.version.groupId.empty";
    private static final String VERSION_ALIAS_EMPTY_EXCEPTION = "error.publish.version.alias.empty";
    private static final String VERSION_EMPTY_EXCEPTION = "error.publish.version.version.empty";
    private static final String ARTIFACT_ID_EMPTY_EXCEPTION = "error.publish.version.artifactId.empty";
    private static final String PUBLISH_VERSION_NOT_EXIST_EXCEPTION = "error.publish.version.not.existed";

    private static final String WEBSOCKET_GENERATE_TAG_COMPARE = "agile-generate-tag-compare-issues";

    private static final String DOING = "doing";

    private static final String DONE = "done";

    private static final String FAILED = "failed";

    private static final String TAG_COMPARE_ADD = "add";

    private static final String TAG_COMPARE_UPDATE = "update";

    @Override
    public PublishVersionVO create(Long projectId, PublishVersionVO publishVersionVO) {
        String version = publishVersionVO.getVersion();
        String groupId = publishVersionVO.getGroupId();
        String artifactId = publishVersionVO.getArtifactId();
        createValidator(publishVersionVO, version, groupId, artifactId);
        publishVersionVO.setProjectId(projectId);
        publishVersionVO.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        if (!StringUtils.isEmpty(version)
                && !StringUtils.isEmpty(groupId)
                && !StringUtils.isEmpty(artifactId)) {
            boolean isExisted = isExisted(projectId, publishVersionVO);
            if (isExisted) {
                PublishVersionDTO dto =
                        queryByGroupIdAndArtifactIdAndVersion(projectId, groupId, artifactId, version);
                //把导入的发布版本战事到列表页
                if (dto != null) {
                    if (dto.getAppService()) {
                        throw new CommonException("error.publish.version.duplicate");
                    } else {
                        dto.setAppService(publishVersionVO.getAppService());
                        dto.setVersionAlias(publishVersionVO.getVersionAlias());
                        if (publishVersionMapper.updateByPrimaryKeySelective(dto) != 1) {
                            throw new CommonException("error.publish.version.update");
                        }
                        return modelMapper.map(publishVersionMapper.selectByPrimaryKey(dto.getId()), PublishVersionVO.class);
                    }
                }
            }
        }
        if (!StringUtils.isEmpty(publishVersionVO.getVersionAlias())
                && checkAlias(projectId, publishVersionVO.getVersionAlias(), null)) {
            throw new CommonException("error.publish.version.alias.duplicate");
        }
        if (StringUtils.isEmpty(publishVersionVO.getStatusCode())) {
            publishVersionVO.setStatusCode(PublishVersionDTO.VERSION_PLANNING);
        }
        PublishVersionDTO dto = modelMapper.map(publishVersionVO, PublishVersionDTO.class);
        if (publishVersionMapper.insertSelective(dto) != 1) {
            throw new CommonException("error.publish.version.insert");
        }
        createPublishVersionTreeClosure(dto.getId(), projectId, dto.getOrganizationId());
        return modelMapper.map(publishVersionMapper.selectByPrimaryKey(dto.getId()), PublishVersionVO.class);
    }

    private PublishVersionDTO queryByGroupIdAndArtifactIdAndVersion(Long projectId,
                                                                    String groupId,
                                                                    String artifactId,
                                                                    String version) {
        AssertUtilsForCommonException.notNull(projectId, "error.publish.version.projectId.null");
        AssertUtilsForCommonException.notEmpty(groupId, GROUP_ID_EMPTY_EXCEPTION);
        AssertUtilsForCommonException.notEmpty(artifactId, ARTIFACT_ID_EMPTY_EXCEPTION);
        AssertUtilsForCommonException.notEmpty(version, VERSION_EMPTY_EXCEPTION);

        PublishVersionDTO dto = new PublishVersionDTO();
        dto.setProjectId(projectId);
        dto.setGroupId(groupId);
        dto.setArtifactId(artifactId);
        dto.setVersion(version);
        List<PublishVersionDTO> list = publishVersionMapper.select(dto);
        PublishVersionDTO result = null;
        if (!ObjectUtils.isEmpty(list)) {
            result = list.get(0);
        }
        return result;
    }

    @Override
    public Boolean isExisted(Long projectId, PublishVersionVO publishVersionVO) {
        String groupId = publishVersionVO.getGroupId();
        String artifactId = publishVersionVO.getArtifactId();
        String version = publishVersionVO.getVersion();
        PublishVersionDTO dto =
                queryByGroupIdAndArtifactIdAndVersion(projectId, groupId, artifactId, version);
        if (publishVersionVO.getId() == null) {
            return dto != null;
        } else {
            if (dto != null) {
                return !dto.getId().equals(publishVersionVO.getId());
            } else {
                return false;
            }
        }
    }

    @Override
    public PublishVersionVO update(Long projectId, Long publishVersionId, PublishVersionVO publishVersionVO) {
        PublishVersionDTO publishVersionDTO = publishVersionMapper.selectByPrimaryKey(publishVersionId);
        if (ObjectUtils.isEmpty(publishVersionDTO)) {
            throw new CommonException(PUBLISH_VERSION_NOT_EXIST_EXCEPTION);
        }
        String groupId = publishVersionVO.getGroupId();
        String version = publishVersionVO.getVersion();
        String artifactId = publishVersionVO.getArtifactId();
        if (!StringUtils.isEmpty(version)
                && !StringUtils.isEmpty(groupId)
                && !StringUtils.isEmpty(artifactId)) {
            PublishVersionVO example = new PublishVersionVO();
            example.setProjectId(projectId);
            example.setId(publishVersionId);
            example.setGroupId(groupId);
            example.setVersion(version);
            example.setArtifactId(artifactId);
            if (isExisted(projectId, example)) {
                throw new CommonException("error.publish.version.duplicate");
            }
        }
        if (!StringUtils.isEmpty(publishVersionVO.getVersionAlias())
                && checkAlias(projectId, publishVersionVO.getVersionAlias(), publishVersionId)) {
            throw new CommonException("error.publish.version.alias.duplicate");
        }
        PublishVersionDTO dto = modelMapper.map(publishVersionVO, PublishVersionDTO.class);
        dto.setId(publishVersionId);
        dto.setProjectId(projectId);
        dto.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        if (publishVersionMapper.updateByPrimaryKey(dto) != 1) {
            throw new CommonException("error.publish.version.update");
        }
        updateTag(publishVersionVO, projectId);
        return modelMapper.map(publishVersionMapper.selectByPrimaryKey(publishVersionDTO.getId()), PublishVersionVO.class);
    }

    private void updateTag(PublishVersionVO publishVersionVO, Long projectId) {
        String appServiceCode = publishVersionVO.getServiceCode();
        String tagName = publishVersionVO.getTagName();
        TagVO tag = null;
        if (!StringUtils.isEmpty(appServiceCode)
                && !StringUtils.isEmpty(tagName)) {
            tag = new TagVO();
            tag.setProjectId(projectId);
            tag.setTagName(tagName);
            tag.setAppServiceCode(appServiceCode);
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Long publishVersionId = publishVersionVO.getId();
        PublishVersionTagRelDTO dto = new PublishVersionTagRelDTO();
        dto.setPublishVersionId(publishVersionId);
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        publishVersionTagRelMapper.delete(dto);
        if (tag != null) {
            publishVersionTreeService.addTag(projectId, organizationId, publishVersionId, new HashSet<>(Arrays.asList(tag)));
        }
    }

    @Override
    public PublishVersionVO query(Long projectId, Long publishVersionId) {
        PublishVersionDTO dto = new PublishVersionDTO();
        dto.setId(publishVersionId);
        dto.setProjectId(projectId);
        PublishVersionDTO publishVersion = publishVersionMapper.selectOne(dto);
        PublishVersionVO result = modelMapper.map(publishVersion, PublishVersionVO.class);
        Long creatorId = publishVersion.getCreatedBy();
        Long updaterId = publishVersion.getLastUpdatedBy();
        Set<Long> userIds = new HashSet<>();
        userIds.add(creatorId);
        userIds.add(updaterId);
        Map<Long, UserMessageDTO> userMessageMap =
                userService.queryUsersMap(new ArrayList<>(userIds), true);
        result.setCreator(userMessageMap.get(creatorId));
        result.setUpdater(userMessageMap.get(updaterId));
        return result;
    }

    @Override
    public void delete(Long projectId, Long publishVersionId) {
        PublishVersionDTO record = new PublishVersionDTO();
        record.setId(publishVersionId);
        record.setProjectId(projectId);
        if (publishVersionMapper.selectOne(record) == null) {
            throw new CommonException(PUBLISH_VERSION_NOT_EXIST_EXCEPTION);
        }
        publishVersionTreeClosureMapper.deleteAssociatedData(publishVersionId, projectId, ConvertUtil.getOrganizationId(projectId));
        if (agilePluginService != null) {
            agilePluginService.deleteAssociatedPublishVersion(publishVersionId);
        }
        PublishVersionTagRelDTO publishVersionTagRelDTO = new PublishVersionTagRelDTO();
        publishVersionTagRelDTO.setProjectId(projectId);
        publishVersionTagRelDTO.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        publishVersionTagRelDTO.setPublishVersionId(publishVersionId);
        publishVersionTagRelMapper.delete(publishVersionTagRelDTO);
        publishVersionMapper.deleteByPrimaryKey(publishVersionId);
    }

    @Override
    public List<PublishVersionVO> parsePom(Long projectId,
                                           String groupIds,
                                           MultipartFile multipartFile,
                                           Long publishVersionId,
                                           Boolean writeBack) {
        ProjectVO project = baseFeignClient.queryProject(projectId).getBody();
        if (project == null) {
            throw new CommonException("error.project.not.existed");
        }
        validateProjectCategories(project);
        List<AppServiceRepVO> appServiceRepList =
                devopsClientOperator.listAppService(projectId, 1, 0, true);
        String fileName = multipartFile.getOriginalFilename();
        if (!fileName.endsWith(".xml")) {
            throw new CommonException("error.illegal.pom.file");
        }
        try {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            InputStream pomInputStream = multipartFile.getInputStream();
            PublishVersionVO self = new PublishVersionVO();
            List<PublishVersionVO> publishVersionList = pomService.parse(groupIds, pomInputStream, appServiceRepList, self);
            if (Boolean.TRUE.equals(writeBack)
                    && !StringUtils.isEmpty(self.getArtifactId())
                    && !StringUtils.isEmpty(self.getGroupId())
                    && !StringUtils.isEmpty(self.getVersion())
                    && !StringUtils.isEmpty(self.getServiceCode())) {
                PublishVersionDTO dto = publishVersionMapper.selectByPrimaryKey(publishVersionId);
                if (dto == null) {
                    throw new CommonException(PUBLISH_VERSION_NOT_EXIST_EXCEPTION);
                }
                PublishVersionVO vo = modelMapper.map(dto, PublishVersionVO.class);
                vo.setGroupId(self.getGroupId());
                vo.setArtifactId(self.getArtifactId());
                vo.setVersion(self.getVersion());
                vo.setServiceCode(self.getServiceCode());
                update(projectId, publishVersionId, vo);
            }
            publishVersionList.forEach(x -> {
                x.setOrganizationId(organizationId);
                if (x.getProjectId() == null) {
                    x.setProjectId(projectId);
                }
            });
            return publishVersionList;
        } catch (IOException
                | SAXException
                | ParserConfigurationException e) {
            throw new CommonException("error.parse.pom", e);
        }
    }

    @Override
    public List<PublishVersionVO> batchCreate(Long projectId,
                                              Long publishVersionId,
                                              List<PublishVersionVO> publishVersionList) {
        List<PublishVersionVO> result = new ArrayList<>();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        if (!ObjectUtils.isEmpty(publishVersionList)) {
            publishVersionList.forEach(x -> {
                PublishVersionVO vo = create(projectId, x);
                result.add(vo);
                Long thisPublishVersionId = vo.getId();
                Long tagProjectId = x.getProjectId();
                String appServiceCode = x.getServiceCode();
                String tagName = x.getTagName();
                if (!ObjectUtils.isEmpty(tagProjectId)
                        && !ObjectUtils.isEmpty(appServiceCode)
                        && !ObjectUtils.isEmpty(tagName)
                        && !ObjectUtils.isEmpty(thisPublishVersionId)) {
                    TagVO tag = new TagVO();
                    tag.setProjectId(tagProjectId);
                    tag.setAppServiceCode(appServiceCode);
                    tag.setTagName(tagName);
                    Set<TagVO> tags = new HashSet<>(Arrays.asList(tag));
                    publishVersionTreeService.addTag(projectId, organizationId, thisPublishVersionId, tags);
                }
            });
        }
        if (publishVersionId != null && !result.isEmpty()) {
            VersionTreeVO vo = new VersionTreeVO();
            vo.setId(publishVersionId);
            List<VersionTreeVO> children = new ArrayList<>();
            vo.setChildren(children);
            result.forEach(x -> {
                VersionTreeVO child = new VersionTreeVO();
                child.setId(x.getId());
                children.add(child);
            });
            publishVersionTreeService.add(projectId, ConvertUtil.getOrganizationId(projectId), vo);
        }
        return result;
    }


    @Override
    public Page<PublishVersionVO> list(Long projectId, PublishVersionVO publishVersionVO, PageRequest pageRequest) {
        return PageHelper.doPageAndSort(pageRequest, () -> publishVersionMapper.listByOptions(projectId, publishVersionVO));
    }

    @Override
    public Boolean checkAlias(Long projectId, String alias, Long publishVersionId) {
        if (StringUtils.isEmpty(alias)) {
            throw new CommonException(VERSION_ALIAS_EMPTY_EXCEPTION);
        }
        PublishVersionDTO dto = new PublishVersionDTO();
        dto.setProjectId(projectId);
        dto.setVersionAlias(alias);
        List<PublishVersionDTO> list = publishVersionMapper.select(dto);
        if (publishVersionId == null) {
            return !list.isEmpty();
        } else {
            if (list.isEmpty()) {
                return false;
            } else {
                Long id = list.get(0).getId();
                return !publishVersionId.equals(id);
            }
        }
    }

    @Override
    public Page<IssueListFieldKVVO> listRelIssueByOption(Long projectId,
                                                         Long organizationId,
                                                         Long publishVersionId,
                                                         SearchVO searchVO,
                                                         PageRequest pageRequest) {
        PublishVersionTreeClosureDTO example = new PublishVersionTreeClosureDTO();
        example.setProjectId(projectId);
        example.setOrganizationId(organizationId);
        example.setAncestorId(publishVersionId);
        Set<Long> publishVersionIds =
                publishVersionTreeClosureMapper
                        .select(example)
                        .stream()
                        .map(PublishVersionTreeClosureDTO::getDescendantId)
                        .collect(Collectors.toSet());
        Page emptyPage = PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
        if (publishVersionIds.isEmpty()) {
            return emptyPage;
        }
        List<IssueDTO> issues =
                publishVersionMapper.selectIssueIds(new HashSet<>(Arrays.asList(projectId)), organizationId, publishVersionIds);
        Set<Long> issueIds  = filterIssueIds(issues, searchVO);
        if (issueIds.isEmpty()) {
            return emptyPage;
        }
        addSearchParam(searchVO, issueIds);
        return issueService.listIssueWithSub(projectId, searchVO, pageRequest, organizationId);
    }

    private Set<Long> filterIssueIds(List<IssueDTO> issues, SearchVO searchVO) {
        Map<String, Object> advancedSearchArgs = searchVO.getAdvancedSearchArgs();
        Set<Long> issueTypeIds = new HashSet<>();
        if (!ObjectUtils.isEmpty(advancedSearchArgs)) {
            List<String> issueTypeIdStr = (List<String>) advancedSearchArgs.get("issueTypeId");
            if (!ObjectUtils.isEmpty(issueTypeIdStr)) {
                issueTypeIdStr.forEach(x -> issueTypeIds.add(Long.valueOf(x)));
                advancedSearchArgs.remove("issueTypeId");
            }
        }
        Map<Long, Set<Long>> parentSonMap = new HashMap<>();
        issues.forEach(x -> {
            if (IssueTypeCode.isSubTask(x.getTypeCode())) {
                Long parentId = x.getParentIssueId();
                if (parentId != null && !Objects.equals(0L, parentId)) {
                    Set<Long> sonSet = parentSonMap.computeIfAbsent(parentId, y -> new HashSet<>());
                    sonSet.add(x.getIssueId());
                }
            } else if (IssueTypeCode.isBug(x.getTypeCode())) {
                Long relateIssueId = x.getRelateIssueId();
                if (relateIssueId != null && !Objects.equals(0L, relateIssueId)) {
                    Set<Long> sonSet = parentSonMap.computeIfAbsent(relateIssueId, y -> new HashSet<>());
                    sonSet.add(x.getIssueId());
                } else {
                    parentSonMap.computeIfAbsent(x.getIssueId(), y -> new HashSet<>());
                }
            } else {
                parentSonMap.computeIfAbsent(x.getIssueId(), y -> new HashSet<>());
            }
        });
        Set<Long> issueIdSet = parentSonMap.keySet();
        if (!issueIdSet.isEmpty() && !issueTypeIds.isEmpty()) {
            issueIdSet =
                    issueMapper.selectByIds(StringUtils.join(issueIdSet, ","))
                            .stream()
                            .filter(x -> issueTypeIds.contains(x.getIssueTypeId()))
                            .map(IssueDTO::getIssueId)
                            .collect(Collectors.toSet());
        }
        Set<Long> issueIds = new HashSet<>();
        for (Map.Entry<Long, Set<Long>> entry : parentSonMap.entrySet()) {
            Long issueId = entry.getKey();
            Set<Long> sonSet = entry.getValue();
            if (issueIdSet.contains(issueId)) {
                issueIds.add(issueId);
                if (!ObjectUtils.isEmpty(sonSet)) {
                    issueIds.addAll(sonSet);
                }
            }

        }
        return issueIds;
    }

    @Override
    @Async
    public void compareTag(Long projectId,
                           Long organizationId,
                           Long publishVersionId,
                           List<TagCompareVO> tagCompareList,
                           String action) {
        String websocketKey = WEBSOCKET_GENERATE_TAG_COMPARE + projectId;
        Long userId = DetailsHelper.getUserDetails().getUserId();
        VersionTagHistoryDTO versionTagHistoryDTO =
                versionTagHistoryService.createDefaultHistory(projectId, organizationId, publishVersionId, action, "publish");
        try {
            PublishVersionDTO dto = new PublishVersionDTO();
            dto.setProjectId(projectId);
            dto.setId(publishVersionId);
            PublishVersionDTO publishVersion = publishVersionMapper.selectOne(dto);
            AssertUtilsForCommonException.notNull(publishVersion, "error.publish.version.null");
            AssertUtilsForCommonException.notEmpty(tagCompareList, "error.tagCompareList.empty");

            Long programId = null;
            if (agilePluginService != null) {
                ProjectVO program = agilePluginService.getProgram(projectId, organizationId);
                if (program != null) {
                    programId = program.getId();
                }
            }
            compareTagAndSendProgress(programId, organizationId, tagCompareList, action, websocketKey, userId, versionTagHistoryDTO);
        } catch (Exception e) {
            TagCompareVO tagCompareVO = new TagCompareVO();
            tagCompareVO.setAction(FAILED);
            tagCompareVO.setProgress(0D);
            tagCompareVO.setMsg(e.getMessage());
            sendProgress(tagCompareVO, userId, websocketKey, 0D);
            versionTagHistoryService.updateStatus(versionTagHistoryDTO, FAILED);
            logger.error("error.compare.tag", e);
        }
    }

    @Override
    public void compareTagAndSendProgress(Long programId,
                                          Long organizationId,
                                          List<TagCompareVO> tagCompareList,
                                          String action,
                                          String websocketKey,
                                          Long userId,
                                          VersionTagHistoryDTO versionTagHistoryDTO) {
        validateTagCompareList(tagCompareList);
        if (!TAG_COMPARE_ADD.equals(action)
                && !TAG_COMPARE_UPDATE.equals(action)) {
            throw new CommonException("error.illegal.action." + action);
        }
        boolean doUpdate = TAG_COMPARE_UPDATE.equals(action);
        List<TagVO> tags = new ArrayList<>();
        Set<Long> allIssueIds = new HashSet<>();
        getIssueIdsByTagsFromDevops(tags, allIssueIds, tagCompareList);

        if (!tags.isEmpty()) {
            Map<Long, Map<Long, IssueDTO>> projectIssueMap = new HashMap<>();
            issueMapper.selectByIds(StringUtils.join(allIssueIds, ","))
                    .forEach(x -> {
                        Long thisProjectId = x.getProjectId();
                        Map<Long, IssueDTO> issueMap = projectIssueMap.computeIfAbsent(thisProjectId, y -> new HashMap<>());
                        issueMap.put(x.getIssueId(), x);
                    });

            int total = tags.size();
            double current = 1D;
            double lastProgress = 0D;
            for (TagVO tag : tags) {
                Long thisProjectId = tag.getProjectId();
                Map<Long, IssueDTO> issueMap = projectIssueMap.get(tag.getProjectId());
                if (issueMap == null) {
                    issueMap = new HashMap<>();
                }
                addTagToIssue(tag, thisProjectId, organizationId, programId, issueMap, doUpdate);
                TagCompareVO tagCompareVO = tag.getTagCompareVO();
                addTagCompareHistory(tagCompareVO, thisProjectId, organizationId, versionTagHistoryDTO.getId());
                tagCompareVO.setAction(DOING);
                tagCompareVO.setProgress(getProgress(current, total));
                lastProgress = sendProgress(tagCompareVO, userId, websocketKey, lastProgress);
                current++;
            }
        }
        TagCompareVO tagCompareVO = new TagCompareVO();
        tagCompareVO.setAction(DONE);
        tagCompareVO.setProgress(1D);
        versionTagHistoryService.updateStatus(versionTagHistoryDTO, DONE);
        sendProgress(tagCompareVO, userId, websocketKey, 1D);
    }

    private void validateTagCompareList(List<TagCompareVO> tagCompareList) {
        tagCompareList.forEach(x -> {
            //targetTag为null时，处理第一次打tag的情况
            AssertUtilsForCommonException.notNull(x.getProjectId(), "error.tagCompare.projectId.null");
            AssertUtilsForCommonException.notNull(x.getAppServiceId(), "error.tagCompare.appServiceId.null");
            AssertUtilsForCommonException.notEmpty(x.getAppServiceCode(), "error.tagCompare.appServiceCode.empty");
            AssertUtilsForCommonException.notEmpty(x.getSourceTag(), "error.tagCompare.sourceTag.empty");
        });
    }

    @Override
    public void deleteTag(Long projectId, String appServiceCode, String tagName) {
        if (ObjectUtils.isEmpty(projectId)
                || ObjectUtils.isEmpty(appServiceCode)
                || ObjectUtils.isEmpty(tagName)) {
            return;
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        deleteTagIssueRel(projectId, organizationId, appServiceCode, tagName);
        deletePublishVersionTagRel(projectId, organizationId, appServiceCode, tagName);
        deleteTagCompareHistory(projectId, organizationId, appServiceCode, tagName);
        if (agilePluginService != null) {
            agilePluginService.deleteProgramTagRel(projectId, organizationId, appServiceCode, tagName);
        }
    }

    @Override
    public void updateStatus(Long projectId,
                             Long publishVersionId,
                             String statusCode,
                             Long objectVersionNumber) {
        if (!PublishVersionDTO.VERSION_PLANNING.equals(statusCode)
                && !PublishVersionDTO.RELEASED.equals(statusCode)) {
            throw new CommonException("error.illegal.publishVersion.statusCode");
        }
        PublishVersionDTO example = new PublishVersionDTO();
        example.setId(publishVersionId);
        example.setProjectId(projectId);
        PublishVersionDTO dto = publishVersionMapper.selectOne(example);
        AssertUtilsForCommonException.notNull(dto, "error.publishVersion.not.existed");
        if (!statusCode.equals(dto.getStatusCode())) {
            dto.setStatusCode(statusCode);
            dto.setObjectVersionNumber(objectVersionNumber);
            if (publishVersionMapper.updateByPrimaryKeySelective(dto) != 1) {
                throw new CommonException("error.publishVersion.update.status");
            }
        }
    }

    @Override
    public List<TagCompareHistoryDTO> tagCompareHistory(Long projectId,
                                                        Long organizationId,
                                                        Long versionId,
                                                        String versionType) {
        return tagCompareHistoryMapper.selectLastCompareHistory(projectId, organizationId, versionId, versionType);
    }

    @Override
    public List<IssueListFieldKVVO> previewIssueFromTag(Long projectId,
                                                        Long organizationId,
                                                        TagCompareVO tagCompareVO) {
        List<TagCompareVO> tagCompareList = Arrays.asList(tagCompareVO);
        validateTagCompareList(tagCompareList);
        Set<Long> issueIds;
        try {
            issueIds =
                    devopsClientOperator.getIssueIdsBetweenTags(projectId,
                            tagCompareVO.getAppServiceId(),
                            tagCompareVO.getSourceTag(),
                            tagCompareVO.getTargetTag());
        } catch (Exception e) {
            throw new CommonException("error.getIssue.from.devops", e);
        }
        if (!ObjectUtils.isEmpty(issueIds)) {
            SearchVO searchVO = new SearchVO();
            Map<String, Object> searchArgs = new LinkedHashMap<>();
            searchVO.setSearchArgs(searchArgs);
            searchArgs.put("tree", false);
            Map<String, Object> otherArgs = new LinkedHashMap<>();
            searchVO.setOtherArgs(otherArgs);
            otherArgs.put("issueIds", new ArrayList<>(issueIds));
            PageRequest pageRequest = new PageRequest(1, 0);
            Sort.Order order = new Sort.Order(Sort.Direction.DESC, "issueNum");
            Sort sort = new Sort(order);
            pageRequest.setSort(sort);
            return issueService.listIssueWithSub(projectId, buildSearchVO(searchVO, issueIds), pageRequest, organizationId)
                    .getContent();
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public List<AppServiceRepVO> activeAppService(Long projectId, Long publishVersionId) {
        List<AppServiceRepVO> appServiceList = devopsClientOperator.listActiveAppService(projectId);
        if (appServiceList.isEmpty()) {
            return appServiceList;
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Set<Long> projectIds = new HashSet<>(Arrays.asList(projectId));
        Set<TagVO> tags = queryTagList(new HashSet<>(Arrays.asList(publishVersionId)), organizationId, projectIds);
        Set<String> appServiceCodes = tags.stream().map(TagVO::getAppServiceCode).collect(Collectors.toSet());
        List<AppServiceRepVO> result = new ArrayList<>();
        appServiceList.forEach(x -> {
            String code = x.getCode();
            if (appServiceCodes.contains(code)) {
                result.add(x);
            }
        });
        return result;
    }

    @Override
    public Set<TagVO> queryTagList(Set<Long> publishVersionIdSet, Long organizationId, Set<Long> projectIds) {
        Set<Long> publishVersionIds =
                publishVersionTreeClosureMapper
                        .selectDescendants(
                                projectIds,
                                organizationId,
                                publishVersionIdSet,
                                null)
                        .stream()
                        .map(PublishVersionTreeClosureDTO::getDescendantId)
                        .collect(Collectors.toSet());
        Set<TagVO> tags = new HashSet<>();
        publishVersionMapper.selectWithTag(publishVersionIds, projectIds, organizationId)
                .forEach(x -> {
                    List<TagVO> tagList = x.getTags();
                    if (!ObjectUtils.isEmpty(tagList)) {
                        tags.addAll(tagList);

                    }
                });
        return tags;
    }

    @Override
    public List<IssueTypeCountVO> issueTypeCount(Long projectId, Long publishVersionId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Set<Long> projectIds = new HashSet<>(Arrays.asList(projectId));
        Set<TagVO> tags = queryTagList(new HashSet<>(Arrays.asList(publishVersionId)), organizationId, projectIds);
        List<IssueTypeCountVO> result = new ArrayList<>();
        if (!tags.isEmpty()) {
            List<IssueTypeCountVO> issueTypeCountList =
                    tagIssueRelMapper.statisticsByIssueType(organizationId, projectId, tags);
            if (issueTypeCountList.isEmpty()) {
                return result;
            }
            Map<Long, Integer> countMap =
                    issueTypeCountList
                            .stream()
                            .collect(Collectors.toMap(IssueTypeCountVO::getIssueTypeId, IssueTypeCountVO::getCount));
            IssueTypeSearchVO issueTypeSearch = new IssueTypeSearchVO();
            issueTypeSearch.setIssueTypeIds(new ArrayList<>(countMap.keySet()));
            List<IssueTypeVO> issueTypeList = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearch);
            issueTypeList.forEach(x -> {
                Long issueTypeId = x.getId();
                Integer count = countMap.get(issueTypeId);
                if (count == null) {
                    return;
                }
                IssueTypeCountVO vo = new IssueTypeCountVO();
                vo.setIssueTypeId(issueTypeId);
                vo.setIssueTypeName(x.getName());
                vo.setCount(count);
                result.add(vo);
            });
        }
        return result;
    }

    private double getProgress(double current, int total) {

        BigDecimal num1 = BigDecimal.valueOf(current);
        BigDecimal num2 = BigDecimal.valueOf(total + 1L);
        return num1.divide(num2, 2, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    private SearchVO buildSearchVO(SearchVO searchVO, Set<Long> issueIds) {
        SearchVO result = new SearchVO();
        BeanUtils.copyProperties(searchVO, result);
        Map<String, Object> searchArgs = result.getSearchArgs();
        if (searchArgs == null) {
            searchArgs = new LinkedHashMap<>();
            result.setSearchArgs(searchArgs);
        }
        searchArgs.put("tree", false);
        Map<String, Object> otherArgs = result.getOtherArgs();
        if (otherArgs == null) {
            otherArgs = new LinkedHashMap<>();
            result.setOtherArgs(otherArgs);
        }
        otherArgs.put("issueIds", new ArrayList<>(issueIds));
        return result;
    }

    private double sendProgress(TagCompareVO tagCompareVO, Long userId, String websocketKey, double lastProgress) {
        if (tagCompareVO.getProgress() != 0 && tagCompareVO.getProgress() < 1 && tagCompareVO.getProgress() - lastProgress < 0.1) {
            return lastProgress;
        }
        try {
            String message = objectMapper.writeValueAsString(tagCompareVO);
            messageClientC7n.sendByUserId(userId, websocketKey, message);
        } catch (JsonProcessingException e) {
            logger.error("parse object to string error: {}", e);
        }
        return tagCompareVO.getProgress();
    }

    private void deleteTagCompareHistory(Long projectId,
                                         Long organizationId,
                                         String appServiceCode,
                                         String tagName) {
        TagCompareHistoryDTO dto = new TagCompareHistoryDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setAppServiceCode(appServiceCode);
        dto.setSource(tagName);
        tagCompareHistoryMapper.delete(dto);
    }

    private void deletePublishVersionTagRel(Long projectId,
                                            Long organizationId,
                                            String appServiceCode,
                                            String tagName) {

        PublishVersionTagRelDTO dto = new PublishVersionTagRelDTO();
        dto.setProjectId(projectId);
        dto.setTagName(tagName);
        dto.setOrganizationId(organizationId);
        dto.setAppServiceCode(appServiceCode);
        publishVersionTagRelMapper.delete(dto);
    }

    private void deleteTagIssueRel(Long projectId,
                                   Long organizationId,
                                   String appServiceCode,
                                   String tagName) {
        TagIssueRelDTO dto = new TagIssueRelDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setAppServiceCode(appServiceCode);
        dto.setTagName(tagName);
        tagIssueRelMapper.delete(dto);
    }

    private void getIssueIdsByTagsFromDevops(List<TagVO> tags,
                                            Set<Long> allIssueIds,
                                            List<TagCompareVO> tagCompareList) {
        for (TagCompareVO tagCompareVO : tagCompareList) {
            String appServiceCode = tagCompareVO.getAppServiceCode();
            String tagName = tagCompareVO.getSourceTag();
            Long projectId = tagCompareVO.getProjectId();
            try {
                Set<Long> issueIds =
                        devopsClientOperator.getIssueIdsBetweenTags(projectId,
                                tagCompareVO.getAppServiceId(),
                                tagCompareVO.getSourceTag(),
                                tagCompareVO.getTargetTag());
                if (!ObjectUtils.isEmpty(issueIds)) {
                    TagVO tagVO = new TagVO();
                    tagVO.setProjectId(projectId);
                    tagVO.setAppServiceCode(appServiceCode);
                    tagVO.setTagName(tagName);
                    tagVO.setIssueIds(issueIds);
                    tagVO.setTagCompareVO(tagCompareVO);
                    tags.add(tagVO);
                    allIssueIds.addAll(issueIds);
                }
            } catch (Exception e) {
                throw new CommonException("error.getIssue.from.devops", e);
            }
        }
    }

    private void addTagCompareHistory(TagCompareVO tagCompareVO,
                                      Long projectId,
                                      Long organizationId,
                                      Long tagOperationHistoryId) {
        TagCompareHistoryDTO dto = new TagCompareHistoryDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setSource(tagCompareVO.getSourceTag());
        dto.setTarget(tagCompareVO.getTargetTag());
        dto.setAppServiceCode(tagCompareVO.getAppServiceCode());
        dto.setVersionTagHistoryId(tagOperationHistoryId);
        tagCompareHistoryMapper.insert(dto);
    }

    private void addTagToIssue(TagVO tag,
                               Long projectId,
                               Long organizationId,
                               Long programId,
                               Map<Long, IssueDTO> issueMap,
                               boolean doUpdate) {
        String appServiceCode = tag.getAppServiceCode();
        String tagName = tag.getTagName();
        Set<Long> issueIds = tag.getIssueIds();
        if (!ObjectUtils.isEmpty(issueIds)) {
            if (doUpdate) {
                //删除旧数据
                tagIssueRelMapper.deleteByIssueIds(projectId, organizationId, issueIds);
            }
            issueIds.forEach(x -> {
                IssueDTO issue = issueMap.get(x);
                if (issue == null) {
                    return;
                }
                String issueTypeCode = issue.getTypeCode();
                if (issueTypeCode == null) {
                    return;
                }
                TagIssueRelDTO dto = new TagIssueRelDTO();
                dto.setIssueId(x);
                dto.setProjectId(projectId);
                dto.setOrganizationId(organizationId);
                dto.setAppServiceCode(appServiceCode);
                dto.setTagName(tagName);
                dto.setTagProjectId(tag.getProjectId());
                if (tagIssueRelMapper.select(dto).isEmpty()) {
                    tagIssueRelMapper.insert(dto);
                }
                if (agilePluginService != null
                        && IssueTypeCode.isStory(issueTypeCode)
                        && programId != null
                        && issue.getFeatureId() != null
                        && !Objects.equals(0L, issue.getFeatureId())) {
                    agilePluginService.addTagToFeature(issue.getFeatureId(), programId, organizationId, tag);
                }
            });
        }
    }

    private void addSearchParam(SearchVO searchVO,
                                Set<Long> issueIds) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (otherArgs == null) {
            otherArgs = new HashMap<>();
            searchVO.setOtherArgs(otherArgs);
        }
        List<String> issueIdList = (List<String>) otherArgs.get("issueIds");
        if (issueIdList == null) {
            issueIdList = new ArrayList<>();
            otherArgs.put("issueIds", issueIdList);
        }
        for (Long issueId : issueIds) {
            issueIdList.add(issueId + "");
        }
        addTreeParamIfNotExisted(searchVO, true);
    }

    private void addTreeParamIfNotExisted(SearchVO searchVO, boolean defaultValue) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (otherArgs == null) {
            otherArgs = new HashMap<>();
            searchVO.setOtherArgs(otherArgs);
        }
        otherArgs.putIfAbsent("tree", defaultValue);
    }

    private void validateProjectCategories(ProjectVO project) {
        Set<String> projectCategoryCodes = new HashSet<>();
        if (!ObjectUtils.isEmpty(project.getCategories())) {
            projectCategoryCodes.addAll(
                    project
                            .getCategories()
                            .stream()
                            .map(ProjectCategoryDTO::getCode)
                            .collect(Collectors.toSet()));
        }
        if (!projectCategoryCodes.contains(ProjectCategory.MODULE_DEVOPS)) {
            throw new CommonException("error.project.category.not.contains.devops");
        }
    }

    private void createPublishVersionTreeClosure(Long id, Long projectId, Long organizationId) {
        PublishVersionTreeClosureDTO dto = buildTreeClosureSelf(id, projectId, organizationId);
        if (publishVersionTreeClosureMapper.select(dto).isEmpty()) {
            publishVersionTreeClosureMapper.insert(dto);
        }
    }

    private PublishVersionTreeClosureDTO buildTreeClosureSelf(Long id, Long projectId, Long organizationId) {
        PublishVersionTreeClosureDTO dto = new PublishVersionTreeClosureDTO();
        dto.setAncestorId(id);
        dto.setDescendantId(id);
        dto.setDescendantParent(0L);
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        return dto;
    }

    private void createValidator(PublishVersionVO publishVersionVO,
                                 String version,
                                 String groupId,
                                 String artifactId) {
        boolean isAppService = Boolean.TRUE.equals(publishVersionVO.getAppService());
        if (isAppService) {
            String versionAlias = publishVersionVO.getVersionAlias();
            if (StringUtils.isEmpty(versionAlias)) {
                throw new CommonException(VERSION_ALIAS_EMPTY_EXCEPTION);
            }
        } else {
            if (StringUtils.isEmpty(version)) {
                throw new CommonException(VERSION_EMPTY_EXCEPTION);
            }
            if (StringUtils.isEmpty(groupId)) {
                throw new CommonException(GROUP_ID_EMPTY_EXCEPTION);
            }
            if (StringUtils.isEmpty(artifactId)) {
                throw new CommonException(ARTIFACT_ID_EMPTY_EXCEPTION);
            }
        }
    }
}
