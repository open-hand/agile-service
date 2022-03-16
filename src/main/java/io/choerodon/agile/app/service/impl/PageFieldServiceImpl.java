package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.annotation.CopyPageField;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.EnumUtil;
import io.choerodon.agile.infra.utils.FieldValueUtil;
import io.choerodon.agile.infra.utils.RankUtil;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
@Service
/**
 * 在高并发的环境，声明式事务与同步锁同时使用的情况下由于事务还没提交导致initPageFieldByOrg重复创建的问题，因此需要把事务隔离级别设置为READ_UNCOMMITTED
 */
@Transactional(rollbackFor = Exception.class, isolation = Isolation.READ_UNCOMMITTED)
public class PageFieldServiceImpl implements PageFieldService {

    private static final String ERROR_PAGECODE_ILLEGAL = "error.pageCode.illegal";
    private static final String ERROR_CONTEXT_ILLEGAL = "error.context.illegal";
    private static final String ERROR_SCHEMECODE_ILLEGAL = "error.schemeCode.illegal";
    private static final String ERROR_FIELDCODE_ILLEGAL = "error.fieldCode.illegal";
    private static final String ERROR_PAGEFIELD_CREATE = "error.pageField.create";
    private static final String ERROR_PAGEFIELD_DELETE = "error.pageField.delete";
    private static final String ERROR_PAGEFIELD_NOTFOUND = "error.pageField.notFound";
    private static final String ERROR_PAGEFIELD_UPDATE = "error.pageField.update";

    @Autowired
    private PageFieldMapper pageFieldMapper;
    @Autowired
    private PageMapper pageMapper;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private ProjectPageFieldMapper projectPageFieldMapper;
    @Autowired
    private FieldOptionService optionService;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired
    private LookupValueMapper lookupValueMapper;
    @Autowired
    protected FieldValueMapper fieldValueMapper;
    @Autowired
    protected ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    protected ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private IssueMapper issueMapper;

    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired
    private BaseFeignClient baseFeignClient;

    @Autowired
    private FieldCascadeRuleService fieldCascadeRuleService;
    @Autowired
    private FieldPermissionService fieldPermissionService;

    @Override
    public PageFieldDTO baseCreate(PageFieldDTO field) {
        if (pageFieldMapper.insert(field) != 1) {
            throw new CommonException(ERROR_PAGEFIELD_CREATE);
        }
        return pageFieldMapper.selectByPrimaryKey(field.getId());
    }

    @Override
    public void baseDelete(Long fieldId) {
        if (pageFieldMapper.deleteByPrimaryKey(fieldId) != 1) {
            throw new CommonException(ERROR_PAGEFIELD_DELETE);
        }
    }

    @Override
    public void baseUpdate(PageFieldDTO pageField) {
        if (pageFieldMapper.updateByPrimaryKeySelective(pageField) != 1) {
            throw new CommonException(ERROR_PAGEFIELD_UPDATE);
        }
    }

    @Override
    public PageFieldDTO baseQueryById(Long organizationId, Long projectId, Long pageFieldId) {
        PageFieldDTO pageField = pageFieldMapper.selectByPrimaryKey(pageFieldId);
        if (pageField == null) {
            throw new CommonException(ERROR_PAGEFIELD_NOTFOUND);
        }
        return pageField;
    }

    @Override
    public Map<String, Object> listQuery(Long organizationId, Long projectId, String pageCode, String context, Long issueTypeId) {
        Map<String, Object> result = new HashMap<>(2);
        if (Boolean.FALSE.equals(EnumUtil.contain(PageCode.class, pageCode))) {
            throw new CommonException(ERROR_PAGECODE_ILLEGAL);
        }
        if (issueTypeId != null && !EnumUtil.contain(ObjectSchemeFieldContext.class, context)) {
            throw new CommonException(ERROR_CONTEXT_ILLEGAL);
        }
        List<PageFieldDTO> pageFields = queryPageField(organizationId, projectId, pageCode, issueTypeId);
        List<PageFieldVO> pageFieldVOS = modelMapper.map(pageFields, new TypeToken<List<PageFieldVO>>() {
        }.getType());
        fillContextName(pageFieldVOS);
        PageDTO select = new PageDTO();
        select.setPageCode(pageCode);
        result.put("name", pageMapper.selectOne(select).getName());
        result.put("content", pageFieldVOS);
        return result;
    }

    /**
     * 填充contextName
     *
     * @param pageFieldVOS
     */
    private void fillContextName(List<PageFieldVO> pageFieldVOS) {
        LookupTypeWithValuesDTO typeWithValues = lookupValueMapper.queryLookupValueByCode(LookupType.CONTEXT);
        Map<String, String> codeMap = typeWithValues.getLookupValues().stream().collect(Collectors.toMap(LookupValueDTO::getValueCode, LookupValueDTO::getName));
        for (PageFieldVO pageFieldVO : pageFieldVOS) {
            String[] contextCodes = pageFieldVO.getContext().split(",");
            List<String> contextNames = new ArrayList<>(contextCodes.length);
            for (String contextCode : contextCodes) {
                contextNames.add(codeMap.get(contextCode));
            }
            pageFieldVO.setContextName(contextNames.stream().collect(Collectors.joining(",")));
        }
    }

    /**
     * 若没有项目层配置则获取组织层配置
     *
     * @param organizationId
     * @param projectId
     * @param pageCode
     * @return
     */
    @Override
    public List<PageFieldDTO> queryPageField(Long organizationId, Long projectId, String pageCode, Long issueTypeId) {
        Boolean created = null;
        if (PageCode.AGILE_ISSUE_CREATE.equals(pageCode)) {
            created = true;
        }
        Boolean edited = null;
        if (PageCode.AGILE_ISSUE_EDIT.equals(pageCode)) {
            edited = true;
        }
        return selectPageField(organizationId, projectId, issueTypeId, created, edited);
    }

    protected List<PageFieldDTO> selectPageField(Long organizationId, Long projectId, Long issueTypeId, Boolean created, Boolean edited) {
        List<PageFieldDTO> pageFields =
                objectSchemeFieldExtendMapper.selectFields(organizationId, projectId, issueTypeId, created, edited);
        if (pageFields.isEmpty()) {
            objectSchemeFieldService.createSystemFieldIfNotExisted(organizationId);
            pageFields =
                    objectSchemeFieldExtendMapper.selectFields(organizationId, projectId, issueTypeId, created, edited);
        }
        addNotSyncField(organizationId, projectId, pageFields, issueTypeId, created, edited);
        if (agilePluginService != null) {
            pageFields = agilePluginService.handlerProgramPageField(projectId,issueTypeId,pageFields, created, edited);
        }
        return filterFieldsByProjectCategories(pageFields, projectId);
    }

    private List<PageFieldDTO> filterFieldsByProjectCategories(List<PageFieldDTO> pageFields, Long projectId) {
        if (projectId == null) {
            return pageFields;
        }
        ProjectVO project = ConvertUtil.queryProject(projectId);
        Set<String> codes = new HashSet<>(ProjectCategory.getProjectCategoryCodes(project));
        if (codes.contains(ProjectCategory.MODULE_AGILE)
                && !codes.contains(ProjectCategory.MODULE_DEVOPS)) {
            return pageFields.stream().filter(x -> !FieldCode.TAG.equals(x.getFieldCode())).collect(Collectors.toList());
        } else {
            return pageFields;
        }
    }

    private void addNotSyncField(Long organizationId,
                                 Long projectId,
                                 List<PageFieldDTO> pageFields,
                                 Long issueTypeId,
                                 Boolean created,
                                 Boolean edited) {
        //查询没有在fd_object_scheme_field_extend表配置的系统字段
        List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.selectNotSyncFieldByFieldConfig(organizationId, projectId, issueTypeId);
        if (CollectionUtils.isEmpty(objectSchemeFieldDTOS)) {
            return;
        }
        String issueTypeCode = issueTypeService.getIssueTypeById(issueTypeId);
        String endRank = pageFields.get(pageFields.size() - 1).getRank();
        for (ObjectSchemeFieldDTO objectSchemeFieldDTO : objectSchemeFieldDTOS) {
            String fieldContext = objectSchemeFieldService.getFieldContext(objectSchemeFieldDTO.getCode());
            List<String> context = Arrays.asList(fieldContext.split(","));
            if (context.contains(issueTypeCode)) {
                boolean checkFieldPageConfig = checkFieldPageConfig(projectId, issueTypeId, objectSchemeFieldDTO.getId(), issueTypeCode, objectSchemeFieldDTO.getCode() ,created ,edited);
                if (Boolean.FALSE.equals(checkFieldPageConfig)) {
                    continue;
                }
                PageFieldDTO pageFieldDTO = modelMapper.map(objectSchemeFieldDTO, PageFieldDTO.class);
                pageFieldDTO.setFieldId(objectSchemeFieldDTO.getId());
                pageFieldDTO.setFieldName(objectSchemeFieldDTO.getName());
                pageFieldDTO.setFieldCode(objectSchemeFieldDTO.getCode());
                pageFieldDTO.setDisplay(true);
                String preRank = RankUtil.genPre(endRank);
                pageFieldDTO.setRank(preRank);
                endRank = preRank;
                pageFields.add(pageFieldDTO);
            }
        }
    }

    private boolean checkFieldPageConfig(Long projectId, Long issueTypeId, Long fieldId, String issueType, String code, Boolean created, Boolean edited) {
        Boolean checkConfig = false;
        ObjectSchemeFieldExtendDTO extendDTO = objectSchemeFieldExtendMapper.selectOne(new ObjectSchemeFieldExtendDTO(projectId, issueTypeId, fieldId));
        if (!Objects.isNull(extendDTO)) {
            return Objects.equals(extendDTO.getCreated(), created) || Objects.equals(extendDTO.getEdited(), edited);
        }
        SystemFieldPageConfig.CommonField commonField = SystemFieldPageConfig.CommonField.queryByField(code);
        if (!ObjectUtils.isEmpty(commonField)) {
            return Objects.equals(commonField.created(), created) || Objects.equals(commonField.edited(), edited);
        }
        if (backlogExpandService != null) {
            return backlogExpandService.checkFieldPageConfig(issueType, code, created, edited);
        }
        return checkConfig;
    }

    @Override
    @CopyPageField
    public PageFieldVO adjustFieldOrder(Long organizationId, Long projectId, String pageCode, AdjustOrderVO adjustOrder) {
        if (Boolean.FALSE.equals(EnumUtil.contain(PageCode.class, pageCode))) {
            throw new CommonException(ERROR_PAGECODE_ILLEGAL);
        }
        PageFieldDTO current = pageFieldMapper.queryByFieldId(organizationId, projectId, pageCode, adjustOrder.getCurrentFieldId());
        PageFieldDTO outset = pageFieldMapper.queryByFieldId(organizationId, projectId, pageCode, adjustOrder.getOutsetFieldId());
        PageFieldDTO update = new PageFieldDTO();
        update.setId(current.getId());
        update.setObjectVersionNumber(current.getObjectVersionNumber());
        if (Boolean.TRUE.equals(adjustOrder.getBefore())) {
            update.setRank(RankUtil.genNext(outset.getRank()));
        } else {
            String rightRank = pageFieldMapper.queryRightRank(organizationId, projectId, pageCode, outset.getRank());
            if (rightRank == null) {
                update.setRank(RankUtil.genPre(outset.getRank()));
            } else {
                update.setRank(RankUtil.between(outset.getRank(), rightRank));
            }
        }
        baseUpdate(update);
        return modelMapper.map(pageFieldMapper.queryByFieldId(organizationId, projectId, pageCode, current.getFieldId()), PageFieldVO.class);
    }

    @Override
    @CopyPageField
    public PageFieldVO update(Long organizationId, Long projectId, String pageCode, Long fieldId, PageFieldUpdateVO updateDTO) {
        if (Boolean.FALSE.equals(EnumUtil.contain(PageCode.class, pageCode))) {
            throw new CommonException(ERROR_PAGECODE_ILLEGAL);
        }
        PageFieldDTO field = pageFieldMapper.queryByFieldId(organizationId, projectId, pageCode, fieldId);
        PageFieldDTO update = modelMapper.map(updateDTO, PageFieldDTO.class);
        update.setId(field.getId());
        baseUpdate(update);
        return modelMapper.map(pageFieldMapper.queryByFieldId(organizationId, projectId, pageCode, fieldId), PageFieldVO.class);
    }

    @Override
    public synchronized void initPageFieldByOrg(Long organizationId) {
        if (pageFieldMapper.listQuery(organizationId, null, null, null).isEmpty()) {
            //查询page
            List<PageDTO> pages = pageMapper.fulltextSearch(organizationId, new PageSearchVO());
            Map<String, Long> pageMap = pages.stream().collect(Collectors.toMap(PageDTO::getPageCode, PageDTO::getId));
            //查询field
            List<ObjectSchemeFieldDTO> fields = objectSchemeFieldMapper.listQuery(organizationId, null, new ObjectSchemeFieldSearchVO(), null);
            Map<String, Map<String, Long>> schemeCodeFieldMap = fields.stream().collect(Collectors.groupingBy(ObjectSchemeFieldDTO::getSchemeCode, Collectors.toMap(ObjectSchemeFieldDTO::getCode, ObjectSchemeFieldDTO::getId)));
            handleInitPageFieldE(organizationId, schemeCodeFieldMap, pageMap);
        }
    }

    private void handleInitPageFieldE(Long organizationId, Map<String, Map<String, Long>> schemeCodeFieldMap, Map<String, Long> pageMap) {
        Class[] clzes = InitPageFieldE.class.getClasses();
        Arrays.asList(clzes).forEach(cls -> {
            List<InitPageFieldVO> initPageFields = modelMapper.map(Arrays.asList(cls.getEnumConstants()), new TypeToken<List<InitPageFieldVO>>() {
            }.getType());
            String rank = RankUtil.mid();
            for (InitPageFieldVO pageField : initPageFields) {
                Map<String, Long> fieldMap = schemeCodeFieldMap.get(pageField.getSchemeCode());
                if (fieldMap == null) {
                    throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
                }
                Long fieldId = fieldMap.get(pageField.getFieldCode());
                if (fieldId == null) {
                    throw new CommonException(ERROR_FIELDCODE_ILLEGAL);
                }
                pageField.setFieldId(fieldId);
                Long pageId = pageMap.get(pageField.getPageCode());
                if (pageId == null) {
                    throw new CommonException(ERROR_PAGECODE_ILLEGAL);
                }
                pageField.setPageId(pageId);
                //设置rank
                pageField.setRank(rank);
                rank = RankUtil.genPre(rank);
            }
            List<PageFieldDTO> pageFields = modelMapper.map(initPageFields, new TypeToken<List<PageFieldDTO>>() {
            }.getType());
            pageFieldMapper.batchInsert(organizationId, null, pageFields);
        });
    }

    @Override
    @CopyPageField
    public void createByFieldWithPro(Long organizationId, Long projectId, ObjectSchemeFieldDTO field) {
        //查询page
        PageSearchVO searchDTO = new PageSearchVO();
        searchDTO.setSchemeCode(field.getSchemeCode());
        List<PageDTO> pages = pageMapper.fulltextSearch(organizationId, searchDTO);
        pages.forEach(page -> {
            //创建pageField
            PageFieldDTO pageField = new PageFieldDTO();
            pageField.setProjectId(projectId);
            pageField.setOrganizationId(organizationId);
            pageField.setDisplay(false);
            pageField.setFieldId(field.getId());
            pageField.setPageId(page.getId());
            String minRank = pageFieldMapper.queryMinRank(organizationId, projectId, page.getPageCode());
            //若没有数据则初始化【修复旧数据】
            if (minRank == null) {
                initPageFieldByOrg(organizationId);
                minRank = pageFieldMapper.queryMinRank(organizationId, projectId, page.getPageCode());
            }
            pageField.setRank(RankUtil.genPre(minRank));
            baseCreate(pageField);
        });
    }

    @Override
    public void createByFieldWithOrg(Long organizationId, ObjectSchemeFieldDTO field) {
        //项目层自定义同样需要创建字段
        List<ProjectPageFieldDTO> projectPageFields = projectPageFieldMapper.queryByOrgId(organizationId);
        //查询page
        PageSearchVO searchDTO = new PageSearchVO();
        searchDTO.setSchemeCode(field.getSchemeCode());
        List<PageDTO> pages = pageMapper.fulltextSearch(organizationId, searchDTO);
        pages.forEach(page -> {
            //组织层创建pageField
            PageFieldDTO pageField = new PageFieldDTO();
            pageField.setOrganizationId(organizationId);
            pageField.setDisplay(false);
            pageField.setFieldId(field.getId());
            pageField.setPageId(page.getId());
            String minRank = pageFieldMapper.queryMinRank(organizationId, null, page.getPageCode());
            //若没有数据则初始化【修复旧数据】
            if (minRank == null) {
                initPageFieldByOrg(organizationId);
                minRank = pageFieldMapper.queryMinRank(organizationId, null, page.getPageCode());
            }
            pageField.setRank(RankUtil.genPre(minRank));
            baseCreate(pageField);
            //项目层创建pageField
            projectPageFields.forEach(projectPageField -> {
                pageField.setId(null);
                pageField.setProjectId(projectPageField.getProjectId());
                baseCreate(pageField);
            });
        });
    }

    @Override
    public void deleteByFieldId(Long fieldId) {
        pageFieldMapper.deleteByFieldId(fieldId);
    }

    @Override
    public List<PageFieldViewVO> queryPageFieldViewList(Long organizationId, Long projectId, PageFieldViewParamVO paramDTO) {
        Long issueTypeId = paramDTO.getIssueTypeId();
        List<PageFieldViewVO> pageFieldViews = queryPageFieldViewsNoPermissionFilter(organizationId, projectId, paramDTO);
        return fieldPermissionService.filterPageFieldViewVO(projectId, organizationId, issueTypeId, pageFieldViews);
    }

    @Override
    public List<PageFieldViewVO> queryPageFieldViewsNoPermissionFilter(Long organizationId,
                                                                       Long projectId,
                                                                       PageFieldViewParamVO paramDTO) {
        Long issueTypeId = paramDTO.getIssueTypeId();
        String pageCode = paramDTO.getPageCode();
        if (Boolean.FALSE.equals(EnumUtil.contain(PageCode.class, pageCode))) {
            throw new CommonException(ERROR_PAGECODE_ILLEGAL);
        }
        if (Boolean.FALSE.equals(EnumUtil.contain(ObjectSchemeCode.class, paramDTO.getSchemeCode()))) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        List<PageFieldDTO> pageFields = queryPageField(organizationId, projectId, pageCode, issueTypeId);
        List<PageFieldViewVO> pageFieldViews = modelMapper.map(pageFields, new TypeToken<List<PageFieldViewVO>>() {
        }.getType());
        objectSchemeFieldService.setDefaultValueObjs(pageFieldViews, projectId, organizationId);
        FieldValueUtil.handleDefaultValue(pageFieldViews);
        return pageFieldViews;
    }

    @Override
    public List<PageFieldViewVO> queryPageFieldViewListWithInstanceId(Long organizationId, Long projectId, Long instanceId, PageFieldViewParamVO paramDTO) {
        List<PageFieldViewVO> pageFieldViews = queryPageFieldViewList(organizationId, projectId, paramDTO);
        //填充value
        fieldValueService.fillValues(organizationId, projectId, instanceId, paramDTO.getSchemeCode(), pageFieldViews);
        fieldCascadeRuleService.filterPageFieldView(organizationId, projectId, paramDTO, instanceId, pageFieldViews);
        pageFieldViews = fieldPermissionService.filterPageFieldViewVO(projectId, organizationId, paramDTO.getIssueTypeId(), pageFieldViews);
        return pageFieldViews;
    }

    @Override
    public Map<Long, Map<String, Object>> queryFieldValueWithIssueIdsForAgileExport(Long organizationId, List<Long> projectIds, List<Long> instanceIds, Boolean isJustStr) {
        return getFieldValueMap(organizationId, projectIds, instanceIds, isJustStr);
    }

    @Override
    public List<PageFieldViewVO> filterRequireFieldByFieldCodes(Long projectId, Long issueTypeId, List<String> fieldCodes) {
        if(CollectionUtils.isEmpty(fieldCodes)){
            return new ArrayList<>();
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        // 查询该问题类型在创建页面显示的字段
        PageFieldViewParamVO pageFieldViewParamVO = new PageFieldViewParamVO();
        pageFieldViewParamVO.setSchemeCode("agile_issue");
        pageFieldViewParamVO.setIssueTypeId(issueTypeId);
        pageFieldViewParamVO.setPageCode("agile_issue_create");
        List<PageFieldViewVO> pageFieldViewVOS = queryPageFieldViewList(organizationId, projectId, pageFieldViewParamVO);
        return pageFieldViewVOS.stream().filter(v -> fieldCodes.contains(v.getFieldCode()) && Objects.equals( Boolean.TRUE, v.getRequired())).collect(Collectors.toList());
    }

    private Map<Long, Map<String, Object>> getFieldValueMap(Long organizationId,
                                                            List<Long> projectIds,
                                                            List<Long> instanceIds,
                                                            Boolean isJustStr) {
        Map<Long, Map<String, Object>> result = new HashMap<>();
        List<FieldValueDTO> values = fieldValueMapper.queryListByInstanceIds(projectIds, instanceIds, "agile_issue", null);
        if (CollectionUtils.isEmpty(values)) {
            return result;
        }
        List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(instanceIds, null, false, false);

        Set<Long> fieldIds = values.stream().map(FieldValueDTO::getFieldId).collect(Collectors.toSet());
        Map<Long, List<IssueDTO>> issueProjectMap = issueDTOList.stream().collect(Collectors.groupingBy(IssueDTO::getProjectId));
        Map<Long, UserDTO> userMap =
                FieldValueUtil.handleUserMap(
                        values
                                .stream()
                                .filter(x -> (FieldType.MEMBER.equals(x.getFieldType()) || FieldType.MULTI_MEMBER.equals(x.getFieldType())))
                                .map(FieldValueDTO::getOptionId)
                                .collect(Collectors.toList()));

        issueProjectMap.keySet().forEach(projectId -> {
            List<IssueDTO> issues = issueProjectMap.get(projectId);
            Set<Long> issueTypeIds = issues.stream().map(IssueDTO::getIssueTypeId).collect(Collectors.toSet());
            List<ObjectSchemeFieldDTO> fieldDTOS = objectSchemeFieldMapper.listFieldWithExtendList(organizationId, projectId, fieldIds, issueTypeIds);
            Map<Long, ObjectSchemeFieldDTO> fieldMap = fieldDTOS.stream().collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity()));
            Map<Long, List<FieldValueDTO>> valuesMap = values.stream().collect(Collectors.groupingBy(FieldValueDTO::getInstanceId));
            for (IssueDTO issue : issues) {
                List<FieldValueDTO> issueValues = valuesMap.get(issue.getIssueId());
                Map<String, Object> codeValueMap = getCodeValueMap(fieldMap, userMap, isJustStr,  issue, issueValues);
                result.put(issue.getIssueId(), codeValueMap);
            }
        });
        return result;
    }

    private Map<String, Object> getCodeValueMap(Map<Long, ObjectSchemeFieldDTO> fieldMap, Map<Long, UserDTO> userMap, Boolean isJustStr, IssueDTO issue, List<FieldValueDTO> issueValues) {
        if (issueValues != null) {
            Map<String, Object> codeValueMap = new HashMap<>();
            Map<Long, List<FieldValueDTO>> valueGroup = issueValues.stream().collect(Collectors.groupingBy(FieldValueDTO::getFieldId));
            valueGroup.forEach((fieldId, fieldValueDTOList) -> {
                ObjectSchemeFieldDTO objectSchemeField = fieldMap.get(fieldId);
                if (objectSchemeField != null) {
                    List<Long> extendIssueTypeIds = objectSchemeField.getExtendFields().stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toList());
                    if (!CollectionUtils.isEmpty(extendIssueTypeIds) && extendIssueTypeIds.contains(issue.getIssueTypeId())) {
                        PageFieldViewVO view = new PageFieldViewVO();
                        view.setExtraConfig(objectSchemeField.getExtraConfig());
                        FieldValueUtil.handleDTO2Value(view, objectSchemeField.getFieldType(), fieldValueDTOList, userMap, isJustStr);
                        codeValueMap.put(objectSchemeField.getCode(), view.getValueStr());
                    }
                }
            });
            return codeValueMap;
        } else {
            return null;
        }
    }
}
