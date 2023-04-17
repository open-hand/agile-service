package io.choerodon.agile.app.service.impl;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import java.util.*;
import java.util.stream.Collectors;

import com.google.common.collect.Lists;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import io.choerodon.agile.api.validator.ProductVersionValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.app.assembler.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.mapper.ProductVersionMapper;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.agile.infra.utils.RedisUtil;
import io.choerodon.agile.infra.utils.SpringBeanUtil;
import io.choerodon.asgard.saga.feign.SagaClient;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * Created by jian_zhang02@163.com on 2018/5/14.
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ProductVersionServiceImpl implements ProductVersionService {

    private static final String INSERT_ERROR = "error.version.insert";
    private static final String UPDATE_ERROR = "error.version.update";
    private static final String AGILE = "Agile:";
    private static final String PIE_CHART = AGILE + "PieChart";
    private static final String FIX_VERSION = "fixVersion";
    private static final String CUSTOM_CHART = AGILE + "CustomChart";

    @Autowired
    private ProductVersionCreateAssembler productVersionCreateAssembler;
    @Autowired
    private ProductVersionUpdateAssembler productVersionUpdateAssembler;
    @Autowired
    private ProductVersionPageAssembler productVersionPageAssembler;
    @Autowired
    private ProductVersionStatisticsAssembler versionStatisticsAssembler;
    @Autowired
    private IssueAssembler issueAssembler;
    @Autowired
    private VersionIssueRelService versionIssueRelService;
    @Autowired
    private ProductVersionDataAssembler versionDataAssembler;
    @Autowired
    private ProductVersionValidator productVersionValidator;
    @Autowired
    private ProductVersionMapper productVersionMapper;

    @Autowired
    private IssueService issueService;

    @Autowired
    private RedisUtil redisUtil;

    @Autowired
    private IProductVersionService iProductVersionService;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private UserService userService;

    private static final String NOT_EQUAL_ERROR = "error.projectId.notEqual";
    private static final String NOT_FOUND = "error.version.notFound";
    private static final String CATEGORY_DONE_CODE = "done";
    private static final String CATEGORY_TODO_CODE = "todo";
    private static final String CATEGORY_DOING_CODE = "doing";
    private static final String REVOKE_ARCHIVED_ERROR = "error.productVersion.revokeArchived";
    private static final String ARCHIVED_ERROR = "error.productVersion.archived";
    private static final String REVOKE_RELEASE_ERROR = "error.productVersion.revokeRelease";

    private SagaClient sagaClient;
    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    public ProductVersionServiceImpl(SagaClient sagaClient) {
        this.sagaClient = sagaClient;
    }

    public void setSagaClient(SagaClient sagaClient) {
        this.sagaClient = sagaClient;
    }

    @Override
    public synchronized ProductVersionDetailVO createVersion(Long projectId, ProductVersionCreateVO versionCreateVO) {
        try {
            if (!projectId.equals(versionCreateVO.getProjectId())) {
                throw new CommonException(NOT_EQUAL_ERROR);
            }
            ProductVersionDTO productVersionDTO = productVersionCreateAssembler.toTarget(versionCreateVO, ProductVersionDTO.class);
            productVersionValidator.checkDate(productVersionDTO);
            productVersionValidator.judgeName(productVersionDTO.getProjectId(), productVersionDTO.getVersionId(), productVersionDTO.getName());
            //设置状态
            productVersionDTO.setStatusCode(ProductVersionService.VERSION_STATUS_CODE_PLANNING);
            //设置编号
            Integer sequence = productVersionMapper.queryMaxSequenceByProject(projectId);
            productVersionDTO.setSequence(sequence == null ? 0 : sequence + 1);
            ProductVersionDetailVO result = new ProductVersionDetailVO();
            ProductVersionDTO query = createBase(productVersionDTO);
            BeanUtils.copyProperties(query, result);
            return result;
        } catch (Exception e) {
            throw new CommonException("error.create.version", e);
        }

    }

    @Override
    public Boolean deleteVersion(Long projectId, Long versionId, Long targetVersionId) {
        productVersionValidator.judgeExist(projectId, targetVersionId);
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        if (targetVersionId != null && !Objects.equals(targetVersionId, 0L)) {
            List<VersionIssueDTO> versionFixIssues = productVersionMapper.queryIssuesByRelationType(projectId, versionId, VERSION_RELATION_TYPE_FIX);
            if (versionFixIssues != null && !versionFixIssues.isEmpty()) {
                iProductVersionService.batchIssueToDestination(projectId, targetVersionId, versionFixIssues, new Date(), customUserDetails.getUserId());
            }
            List<VersionIssueDTO> versionInfIssues = productVersionMapper.queryIssuesByRelationType(projectId, versionId, VERSION_RELATION_TYPE_INFLUENCE);
            if (versionInfIssues != null && !versionInfIssues.isEmpty()) {
                iProductVersionService.batchIssueToDestination(projectId, targetVersionId, versionInfIssues, new Date(), customUserDetails.getUserId());
            }
        }
        versionIssueRelService.deleteByVersionId(projectId, versionId);
        AgilePluginService agilePluginService = SpringBeanUtil.getExpandBean(AgilePluginService.class);
        if (agilePluginService != null) {
            agilePluginService.deleteProgramVersionRel(projectId, versionId);
        }
        return simpleDeleteVersion(projectId, versionId);
    }

    private Boolean simpleDeleteVersion(Long projectId, Long versionId) {
        try {
            ProductVersionDTO version = new ProductVersionDTO();
            version.setProjectId(projectId);
            version.setVersionId(versionId);
            ProductVersionDTO versionDTO = productVersionMapper.selectOne(version);
            if (versionDTO == null) {
                throw new CommonException(NOT_FOUND);
            }
            return iProductVersionService.delete(versionDTO);
        } catch (Exception e) {
            throw new CommonException("error.simple.delete.version", e);
        }
    }

    @Override
    public ProductVersionDetailVO updateVersion(Long projectId, Long versionId, ProductVersionUpdateVO versionUpdateVO, List<String> fieldList) {
        if (!projectId.equals(versionUpdateVO.getProjectId())) {
            throw new CommonException(NOT_EQUAL_ERROR);
        }
        versionUpdateVO.setVersionId(versionId);
        ProductVersionDTO productVersionDTO = productVersionUpdateAssembler.toTarget(versionUpdateVO, ProductVersionDTO.class);
        productVersionValidator.checkDate(productVersionDTO);
        productVersionValidator.judgeName(productVersionDTO.getProjectId(), productVersionDTO.getVersionId(), productVersionDTO.getName());
        productVersionDTO.setVersionId(versionId);
        return productVersionUpdateAssembler.toTarget(updateByFieldList(productVersionDTO, fieldList), ProductVersionDetailVO.class);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Page<ProductVersionPageVO> queryByProjectId(Long projectId, PageRequest pageRequest, SearchVO searchVO) {
        //过滤查询和排序
        Page<Long> versionIds = PageHelper.doPageAndSort(pageRequest, () -> productVersionMapper.
                queryVersionIdsByProjectId(projectId, searchVO.getSearchArgs(),
                        searchVO.getAdvancedSearchArgs(), searchVO.getContents()));
        if ((versionIds.getContent() != null) && !versionIds.getContent().isEmpty()) {
            List<Long> content = versionIds.getContent();
            List<ProductVersionPageVO> productVersionPageVOS = productVersionPageAssembler.toTargetList(productVersionMapper.
                    queryVersionByIds(projectId, content), ProductVersionPageVO.class);
            if (!CollectionUtils.isEmpty(productVersionPageVOS)) {
                List<Long> userIds = productVersionPageVOS.stream().map(ProductVersionPageVO::getCreatedBy).collect(toList());
                Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(Lists.newArrayList(userIds), true);
                productVersionPageVOS.forEach(productVersionPageVO ->
                        productVersionPageVO.setCreationUser(usersMap.get(productVersionPageVO.getCreatedBy())));
            }
            AgilePluginService agilePluginService = SpringBeanUtil.getExpandBean(AgilePluginService.class);
            if(agilePluginService != null){
                agilePluginService.settingProgramVersions(productVersionPageVOS,projectId,content);
            }
            return PageUtil.buildPageInfoWithPageInfoList(versionIds, productVersionPageVOS);
        } else {
            return new Page<>();
        }
    }

    @Override
    public Boolean repeatName(Long projectId, String name) {
        return productVersionMapper.isRepeatName(projectId, name);
    }

    @Override
    public List<ProductVersionDataVO> queryVersionByProjectId(Long projectId) {
        List<ProductVersionDataVO> productVersions = versionDataAssembler.toTargetList(productVersionMapper.queryVersionByProjectId(projectId), ProductVersionDataVO.class);
        if (!productVersions.isEmpty()) {
            List<Long> productVersionIds = productVersions.stream().map(ProductVersionDataVO::getVersionId).collect(toList());
            Map<String, List<Long>> statusMap = projectConfigService.queryStatusByProjectId(projectId, null, SchemeApplyType.AGILE)
                    .stream().collect(Collectors.groupingBy(StatusVO::getType, Collectors.mapping(StatusVO::getId, Collectors.toList())));
            List<Long> done = statusMap.get(CATEGORY_DONE_CODE);
            Boolean condition = done != null && !done.isEmpty();
            Map<Long, Integer> doneIssueCountMap = Boolean.TRUE.equals(condition) ? productVersionMapper.queryIssueCount(projectId, productVersionIds, done).stream().collect(toMap(IssueCountDTO::getId, IssueCountDTO::getIssueCount)) : null;
            Map<Long, Integer> issueCountMap = productVersionMapper.queryIssueCount(projectId, productVersionIds, null).stream().collect(toMap(IssueCountDTO::getId, IssueCountDTO::getIssueCount));
            Map<Long, Integer> notEstimateMap = productVersionMapper.queryNotEstimate(projectId, productVersionIds).stream().collect(toMap(IssueCountDTO::getId, IssueCountDTO::getIssueCount));
            Map<Long, Integer> totalEstimateMap = productVersionMapper.queryTotalEstimate(projectId, productVersionIds).stream().collect(toMap(IssueCountDTO::getId, IssueCountDTO::getIssueCount));
            productVersions.forEach(productVersion -> {
                productVersion.setIssueCount(issueCountMap.get(productVersion.getVersionId()));
                productVersion.setDoneIssueCount(Boolean.TRUE.equals(condition) ? doneIssueCountMap.get(productVersion.getVersionId()) : null);
                productVersion.setNotEstimate(notEstimateMap.get(productVersion.getVersionId()));
                productVersion.setTotalEstimate(totalEstimateMap.get(productVersion.getVersionId()));
            });
        }
        return productVersions;
    }

    @Override
    public ProductVersionStatisticsVO queryVersionStatisticsByVersionId(Long projectId, Long versionId) {
        ProductVersionStatisticsVO productVersionStatisticsVO = versionStatisticsAssembler.toTarget(productVersionMapper.queryVersionStatisticsByVersionId(projectId, versionId), ProductVersionStatisticsVO.class);
        List<StatusVO> statusMapVOS = projectConfigService.queryStatusByProjectId(projectId, null, SchemeApplyType.AGILE);
        Map<String, List<Long>> statusIdMap = statusMapVOS.stream().collect(Collectors.groupingBy(StatusVO::getType, Collectors.mapping(StatusVO::getId, Collectors.toList())));
        Map<String, List<StatusVO>> statusMap = statusMapVOS.stream().collect(Collectors.groupingBy(StatusVO::getType));
        UserDTO userDTO = userService.queryUserNameByOption(productVersionStatisticsVO.getCreatedBy(), false);
        productVersionStatisticsVO.setCreationUser(userDTO);
        productVersionStatisticsVO.setTodoIssueCount(statusIdMap.get(CATEGORY_TODO_CODE) != null && !statusIdMap.get(CATEGORY_TODO_CODE).isEmpty() ? productVersionMapper.queryStatusIssueCount(statusIdMap.get(CATEGORY_TODO_CODE), projectId, versionId) : 0);
        productVersionStatisticsVO.setDoingIssueCount(statusIdMap.get(CATEGORY_DOING_CODE) != null && !statusIdMap.get(CATEGORY_DOING_CODE).isEmpty() ? productVersionMapper.queryStatusIssueCount(statusIdMap.get(CATEGORY_DOING_CODE), projectId, versionId) : 0);
        productVersionStatisticsVO.setDoneIssueCount(statusIdMap.get(CATEGORY_DONE_CODE) != null && !statusIdMap.get(CATEGORY_DONE_CODE).isEmpty() ? productVersionMapper.queryStatusIssueCount(statusIdMap.get(CATEGORY_DONE_CODE), projectId, versionId) : 0);
        productVersionStatisticsVO.setIssueCount(productVersionStatisticsVO.getTodoIssueCount() + productVersionStatisticsVO.getDoingIssueCount() + productVersionStatisticsVO.getDoneIssueCount());
        productVersionStatisticsVO.setTodoStatuses(statusMap.get(CATEGORY_TODO_CODE));
        productVersionStatisticsVO.setDoingStatuses(statusMap.get(CATEGORY_DOING_CODE));
        productVersionStatisticsVO.setDoneStatuses(statusMap.get(CATEGORY_DONE_CODE));
        return productVersionStatisticsVO;
    }

    @Override
    public List<IssueListVO> queryIssueByVersionIdAndStatusCode(Long projectId, Long versionId, String statusCode, Long organizationId, SearchVO searchVO) {
        //处理用户搜索
        Boolean condition = issueService.handleSearchUser(searchVO, projectId);
        if (Boolean.TRUE.equals(condition)) {
            Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
            Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
            Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
            List<Long> filterStatusIds = new ArrayList<>();
            if (statusCode != null) {
                for (Map.Entry<Long, StatusVO> entry : statusMapDTOMap.entrySet()) {
                    if (statusCode.equals(statusMapDTOMap.get(entry.getKey()).getType())) {
                        filterStatusIds.add(entry.getKey());
                    }
                }
            }
            return issueAssembler.issueDoToIssueListDto(productVersionMapper.queryIssueByVersionIdAndStatusCode(projectId, versionId, statusCode, filterStatusIds, searchVO), priorityMap, statusMapDTOMap, issueTypeDTOMap);
        } else {
            return new ArrayList<>();

        }
    }

    @Override
    public VersionMessageVO queryReleaseMessageByVersionId(Long projectId, Long versionId) {
        VersionMessageVO versionReleaseMessage = new VersionMessageVO();
        versionReleaseMessage.setFixIssueCount(productVersionMapper.queryNotDoneIssueCount(projectId, versionId));
        versionReleaseMessage.setVersionNames(versionStatisticsAssembler.
                toTargetList(productVersionMapper.queryPlanVersionNames(projectId, versionId), ProductVersionNameVO.class));
        return versionReleaseMessage;
    }

    @Override
    public ProductVersionDetailVO releaseVersion(Long projectId, ProductVersionReleaseVO productVersionRelease) {
        if (!Objects.equals(projectId, productVersionRelease.getProjectId())) {
            throw new CommonException(NOT_EQUAL_ERROR);
        }
        productVersionValidator.isRelease(projectId, productVersionRelease);
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        if (productVersionRelease.getTargetVersionId() != null && !Objects.equals(productVersionRelease.getTargetVersionId(), 0L)) {
            List<VersionIssueDTO> incompleteIssues = productVersionMapper.queryIncompleteIssues(projectId, productVersionRelease.getVersionId());
            if (!incompleteIssues.isEmpty()) {
                versionIssueRelService.deleteIncompleteIssueByVersionId(projectId, productVersionRelease.getVersionId());
                iProductVersionService.batchIssueToDestination(projectId, productVersionRelease.getTargetVersionId(), incompleteIssues, new Date(), customUserDetails.getUserId());
            }
        }
        release(projectId, productVersionRelease.getVersionId(), productVersionRelease.getReleaseDate());
        return versionDataAssembler.toTarget(productVersionMapper.selectByPrimaryKey(productVersionRelease.getVersionId()), ProductVersionDetailVO.class);
    }

    @Override
    public ProductVersionDetailVO revokeReleaseVersion(Long projectId, Long versionId) {
        ProductVersionDTO versionDTO = new ProductVersionDTO();
        versionDTO.setProjectId(projectId);
        versionDTO.setVersionId(versionId);
        ProductVersionDTO version = productVersionCreateAssembler.toTarget(productVersionMapper.selectOne(versionDTO), ProductVersionDTO.class);
        if (version == null || !Objects.equals(version.getStatusCode(), ProductVersionService.VERSION_STATUS_CODE_RELEASED)) {
            throw new CommonException(REVOKE_RELEASE_ERROR);
        }
        version.setOldStatusCode(version.getStatusCode());
        version.setStatusCode(VERSION_STATUS_CODE_PLANNING);
        return productVersionUpdateAssembler.toTarget(updateBase(version), ProductVersionDetailVO.class);
    }

    @Override
    public VersionMessageVO queryDeleteMessageByVersionId(Long projectId, Long versionId) {
        VersionMessageVO versionDeleteMessage = new VersionMessageVO();
        versionDeleteMessage.setAgileIssueCount(productVersionMapper.queryIssueCountByApplyType(projectId, versionId, SchemeApplyType.AGILE));
        versionDeleteMessage.setTestCaseCount(productVersionMapper.queryIssueCountByApplyType(projectId, versionId, SchemeApplyType.TEST));
        versionDeleteMessage.setVersionNames(versionStatisticsAssembler.
                toTargetList(productVersionMapper.queryVersionNames(projectId, versionId), ProductVersionNameVO.class));
        return versionDeleteMessage;
    }

    @Override
    public List<ProductVersionNameVO> queryNameByOptions(Long projectId, List<String> statusCodes) {
        return versionStatisticsAssembler.toTargetList(productVersionMapper.queryNameByOptions(projectId, statusCodes), ProductVersionNameVO.class);
    }

    @Override
    public List<ProductVersionVO> listByProjectId(Long projectId) {
        return modelMapper.map(productVersionMapper.listByProjectId(projectId), new TypeToken<List<ProductVersionVO>>() {
        }.getType());
    }

    @Override
    public ProductVersionDetailVO archivedVersion(Long projectId, Long versionId) {
        ProductVersionDTO versionDTO = new ProductVersionDTO();
        versionDTO.setProjectId(projectId);
        versionDTO.setVersionId(versionId);
        ProductVersionDTO version = productVersionCreateAssembler.toTarget(productVersionMapper.selectOne(versionDTO), ProductVersionDTO.class);
        if (version == null || Objects.equals(version.getStatusCode(), VERSION_STATUS_CODE_ARCHIVED)) {
            throw new CommonException(ARCHIVED_ERROR);
        }
        version.setOldStatusCode(version.getStatusCode());
        version.setStatusCode(VERSION_STATUS_CODE_ARCHIVED);
        return productVersionUpdateAssembler.toTarget(updateBase(version), ProductVersionDetailVO.class);
    }

    @Override
    public ProductVersionDetailVO revokeArchivedVersion(Long projectId, Long versionId) {
        ProductVersionDTO versionDTO = new ProductVersionDTO();
        versionDTO.setProjectId(projectId);
        versionDTO.setVersionId(versionId);
        ProductVersionDTO version = productVersionCreateAssembler.toTarget(productVersionMapper.selectOne(versionDTO), ProductVersionDTO.class);
        if (version == null || !Objects.equals(version.getStatusCode(), VERSION_STATUS_CODE_ARCHIVED)) {
            throw new CommonException(REVOKE_ARCHIVED_ERROR);
        }
        version.setStatusCode(version.getOldStatusCode());
        version.setOldStatusCode(VERSION_STATUS_CODE_ARCHIVED);
        return productVersionUpdateAssembler.toTarget(updateBase(version), ProductVersionDetailVO.class);
    }

    @Override
    public ProductVersionDetailVO queryVersionByVersionId(Long projectId, Long versionId) {
        ProductVersionDTO productVersionDTO = new ProductVersionDTO();
        productVersionDTO.setProjectId(projectId);
        productVersionDTO.setVersionId(versionId);
        return versionDataAssembler.toTarget(productVersionMapper.selectOne(productVersionDTO), ProductVersionDetailVO.class);
    }

    @Override
    public List<Long> listIds(Long projectId) {
        return productVersionMapper.listIds();
    }

    @Override
    public synchronized ProductVersionPageVO dragVersion(Long projectId, VersionSequenceVO versionSequenceVO) {
        if (versionSequenceVO.getAfterSequence() == null && versionSequenceVO.getBeforeSequence() == null) {
            throw new CommonException("error.dragVersion.noSequence");
        }
        ProductVersionDTO productVersionDTO = modelMapper.map(queryVersionByProjectIdAndVersionId(
                versionSequenceVO.getVersionId(), projectId), ProductVersionDTO.class);
        if (productVersionDTO == null) {
            throw new CommonException(NOT_FOUND);
        } else {
            if (versionSequenceVO.getAfterSequence() == null) {
                Integer maxSequence = productVersionMapper.queryMaxAfterSequence(versionSequenceVO.getBeforeSequence(), projectId);
                versionSequenceVO.setAfterSequence(maxSequence);
            } else if (versionSequenceVO.getBeforeSequence() == null) {
                Integer minSequence = productVersionMapper.queryMinBeforeSequence(versionSequenceVO.getAfterSequence(), projectId);
                versionSequenceVO.setBeforeSequence(minSequence);
            }
            handleSequence(versionSequenceVO, projectId, productVersionDTO);
        }
        return productVersionPageAssembler.toTarget(queryVersionByProjectIdAndVersionId(
                versionSequenceVO.getVersionId(), projectId), ProductVersionPageVO.class);
    }

    private void handleSequence(VersionSequenceVO versionSequenceVO, Long projectId, ProductVersionDTO productVersionDTO) {
        if (versionSequenceVO.getBeforeSequence() == null) {
            productVersionDTO.setSequence(versionSequenceVO.getAfterSequence() + 1);
            updateBase(productVersionDTO);
        } else if (versionSequenceVO.getAfterSequence() == null) {
            if (productVersionDTO.getSequence() > versionSequenceVO.getBeforeSequence()) {
                Integer add = productVersionDTO.getSequence() - versionSequenceVO.getBeforeSequence();
                if (add > 0) {
                    productVersionDTO.setSequence(versionSequenceVO.getBeforeSequence() - 1);
                    updateBase(productVersionDTO);
                } else {
                    batchUpdateSequence(versionSequenceVO.getBeforeSequence(), projectId,
                            productVersionDTO.getSequence() - versionSequenceVO.getBeforeSequence() + 1, productVersionDTO.getVersionId());
                }
            }
        } else {
            Integer sequence = versionSequenceVO.getAfterSequence() + 1;
            productVersionDTO.setSequence(sequence);
            updateBase(productVersionDTO);
            Integer update = sequence - versionSequenceVO.getBeforeSequence();
            if (update >= 0) {
                batchUpdateSequence(versionSequenceVO.getBeforeSequence(), projectId, update + 1, productVersionDTO.getVersionId());
            }
        }
    }


    private ProductVersionDTO queryVersionByProjectIdAndVersionId(Long versionId, Long projectId) {
        ProductVersionDTO productVersionDTO = new ProductVersionDTO();
        productVersionDTO.setVersionId(versionId);
        productVersionDTO.setProjectId(projectId);
        return productVersionMapper.selectOne(productVersionDTO);
    }

    @Override
    public VersionIssueCountVO queryByCategoryCode(Long projectId, Long versionId) {
        return modelMapper.map(productVersionMapper.queryVersionStatisticsByVersionId(projectId, versionId), VersionIssueCountVO.class);
    }


    @Override
    public ProductVersionDTO createBase(ProductVersionDTO versionDTO) {
        if (productVersionMapper.insertSelective(versionDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        redisUtil.deleteRedisCache(new String[]{PIE_CHART + versionDTO.getProjectId() + ':' + FIX_VERSION + "*"});
        return productVersionMapper.selectByPrimaryKey(versionDTO.getVersionId());
    }

    @Override
    public ProductVersionDTO updateByFieldList(ProductVersionDTO versionDTO, List<String> fieldList) {
        if (productVersionMapper.updateOptional(versionDTO, fieldList.toArray(new String[0])) != 1) {
            throw new CommonException(UPDATE_ERROR);
        }
        redisUtil.deleteRedisCache(new String[]{
                PIE_CHART + versionDTO.getProjectId() + ':' + FIX_VERSION + "*",
                CUSTOM_CHART + versionDTO.getProjectId() + ":" + "*"
        });
        return productVersionMapper.selectByPrimaryKey(versionDTO.getVersionId());
    }

    @Override
    public Boolean release(Long projectId, Long versionId, Date releaseDate) {
        productVersionMapper.releaseVersion(projectId, versionId, releaseDate);
        return true;
    }

    @Override
    public ProductVersionDTO updateBase(ProductVersionDTO versionDTO) {
        if (productVersionMapper.updateByPrimaryKeySelective(versionDTO) != 1) {
            throw new CommonException(UPDATE_ERROR);
        }
        redisUtil.deleteRedisCache(new String[]{
                PIE_CHART + versionDTO.getProjectId() + ':' + FIX_VERSION + "*",
                CUSTOM_CHART + versionDTO.getProjectId() + ":" + "*"
        });
        return productVersionMapper.selectByPrimaryKey(versionDTO.getVersionId());
    }

    @Override
    public int deleteByVersionIds(Long projectId, List<Long> versionIds) {
        redisUtil.deleteRedisCache(new String[]{PIE_CHART + projectId + ':' + FIX_VERSION + "*"});
        return productVersionMapper.deleteByVersionIds(projectId, versionIds);
    }

    @Override
    public int batchUpdateSequence(Integer sequence, Long projectId, Integer add, Long versionId) {
        return productVersionMapper.batchUpdateSequence(sequence, projectId, add, versionId);
    }

    @Override
    public List<TestVersionFixVO> queryByVersionId() {
        List<TestVersionFixVO> testVersionFixVOList = productVersionMapper.queryByVersionId();
        if (testVersionFixVOList != null && !testVersionFixVOList.isEmpty()) {
            return testVersionFixVOList;
        } else {
            return new ArrayList<>();
        }
    }

}
