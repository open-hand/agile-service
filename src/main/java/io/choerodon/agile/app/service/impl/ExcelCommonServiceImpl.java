package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.ProductVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.ExcelImportTemplate;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ExcelUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/10
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ExcelCommonServiceImpl implements ExcelCommonService {

    private static final int PREDEFINED_VALUE_START_ROW = 1;
    private static final int PREDEFINED_VALUE_END_ROW = 500;
    private static final String VERSION_PLANNING = "version_planning";

    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private IssueService issueService;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private ProductVersionMapper productVersionMapper;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private BaseFeignClient baseFeignClient;

    @Override
    public PredefinedDTO processLabelPredefined(Long projectId,
                                                ExcelImportTemplate.Cursor cursor,
                                                List<String> fieldCodes) {
        int col = fieldCodes.indexOf(FieldCode.LABEL);
        if (col == -1) {
            return null;
        }
        List<String> values =
                issueLabelMapper
                        .selectByProjectIds(Arrays.asList(projectId))
                        .stream()
                        .map(IssueLabelDTO::getLabelName)
                        .collect(Collectors.toList());
        return new PredefinedDTO(values,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.LABEL,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public PredefinedDTO processEpicOrFeaturePredefined(Long organizationId,
                                                        Long projectId,
                                                        boolean withFeature,
                                                        ExcelImportTemplate.Cursor cursor,
                                                        List<String> fieldCodes) {
        if (withFeature) {
            int col = fieldCodes.indexOf(FieldCode.FEATURE);
            if (col == -1) {
                return null;
            }
            List<SubFeatureVO> features = agilePluginService.listFeature(organizationId, projectId);
            List<String> featureSummary = features.stream().map(SubFeatureVO::getSummary).collect(Collectors.toList());
            return new PredefinedDTO(featureSummary,
                    PREDEFINED_VALUE_START_ROW,
                    PREDEFINED_VALUE_END_ROW,
                    col,
                    col,
                    FieldCode.FEATURE,
                    cursor.getAndIncreaseSheetNum());
        } else {
            int col = fieldCodes.indexOf(FieldCode.EPIC);
            if (col == -1) {
                return null;
            }
            List<String> values = new ArrayList<>(getEpicMap(projectId).keySet());
            values.sort(String.CASE_INSENSITIVE_ORDER);
            return new PredefinedDTO(values,
                    PREDEFINED_VALUE_START_ROW,
                    PREDEFINED_VALUE_END_ROW,
                    col,
                    col,
                    FieldCode.EPIC,
                    cursor.getAndIncreaseSheetNum());
        }
    }

    @Override
    public Map<String, Long> getEpicMap(Long projectId) {
        Map<String, Long> epicMap = new HashMap<>();
        List<EpicDataVO> epics = issueService.listEpic(projectId);
        epics.forEach(e -> {
            String epicName = e.getEpicName();
            if (ObjectUtils.isEmpty(epicMap.get(epicName))) {
                epicMap.put(epicName, e.getIssueId());
            }
        });
        return epicMap;
    }

    @Override
    public PredefinedDTO buildPredefinedByFieldCodeAndValues(ExcelImportTemplate.Cursor cursor,
                                                             List<String> fieldCodes,
                                                             List<String> values,
                                                             String fieldCode) {
        int col = fieldCodes.indexOf(fieldCode);
        if (col == -1) {
            return null;
        }
        return new PredefinedDTO(values,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                fieldCode,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public PredefinedDTO processSprintPredefined(Long projectId,
                                                 ExcelImportTemplate.Cursor cursor,
                                                 List<String> fieldCodes) {
        int col = fieldCodes.indexOf(FieldCode.SPRINT);
        if (col == -1) {
            return null;
        }
        List<String> sprintList =
                sprintMapper.selectNotDoneByProjectId(projectId)
                        .stream()
                        .map(SprintDTO::getSprintName)
                        .collect(Collectors.toList());
        return new PredefinedDTO(sprintList,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.SPRINT,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public PredefinedDTO processComponentPredefined(Long projectId,
                                                    ExcelImportTemplate.Cursor cursor,
                                                    List<String> fieldCodes) {
        int col = fieldCodes.indexOf(FieldCode.COMPONENT);
        if (col == -1) {
            return null;
        }
        List<String> componentList =
                issueComponentMapper.selectByProjectId(projectId)
                        .stream()
                        .map(IssueComponentDTO::getName)
                        .collect(Collectors.toList());
        return new PredefinedDTO(componentList,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.COMPONENT,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public PredefinedDTO processVersionPredefined(Long projectId,
                                                  ExcelImportTemplate.Cursor cursor,
                                                  List<String> fieldCodes) {
        int col = fieldCodes.indexOf(FieldCode.FIX_VERSION);
        if (col == -1) {
            return null;
        }
        List<ProductVersionCommonDTO> productVersionCommons = productVersionMapper.listByProjectId(projectId);
        List<String> versionList = new ArrayList<>();
        productVersionCommons.forEach(p -> {
            String statusCode = p.getStatusCode();
            if (VERSION_PLANNING.equals(statusCode)) {
                versionList.add(p.getName());
            }
        });
        return new PredefinedDTO(versionList,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.FIX_VERSION,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public PredefinedDTO processInfluenceVersionPredefined(Long projectId,
                                                           ExcelImportTemplate.Cursor cursor,
                                                           List<String> fieldCodes) {
        int col = fieldCodes.indexOf(FieldCode.INFLUENCE_VERSION);
        if (col == -1) {
            return null;
        }
        List<ProductVersionCommonDTO> productVersionCommons = productVersionMapper.listByProjectId(projectId);
        List<String> versionList = new ArrayList<>();
        productVersionCommons.forEach(p -> {
            String statusCode = p.getStatusCode();
            if (VERSION_PLANNING.equals(statusCode)) {
                versionList.add(p.getName());
            }
        });
        return new PredefinedDTO(versionList,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.INFLUENCE_VERSION,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public PredefinedDTO processPriorityPredefined(Long organizationId,
                                                   ExcelImportTemplate.Cursor cursor,
                                                   List<String> fieldCodes) {
        int col = fieldCodes.indexOf(FieldCode.PRIORITY);
        if (col == -1) {
            return null;
        }
        List<PriorityVO> priorityVOList = priorityService.queryByOrganizationIdList(organizationId);
        List<String> priorityList =
                priorityVOList
                        .stream()
                        .filter(p -> Boolean.TRUE.equals(p.getEnable()))
                        .map(PriorityVO::getName)
                        .collect(Collectors.toList());
        return new PredefinedDTO(priorityList,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.PRIORITY,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public PredefinedDTO processIssueStatusPredefined(Long organizationId,
                                                      Long projectId,
                                                      ExcelImportTemplate.Cursor cursor,
                                                      List<String> fieldCodes) {
        int col = fieldCodes.indexOf(FieldCode.ISSUE_STATUS);
        if (col == -1) {
            return null;
        }
        List<ProjectStatusVO> projectStatusVOList = statusMapper.listStatusByProjectId(projectId, organizationId,null);
        List<String> values = new ArrayList<>();
        projectStatusVOList.forEach(i -> values.add(i.getName()));
        return new PredefinedDTO(values,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.ISSUE_STATUS,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public PredefinedDTO processIssueProductPredefined(Long organizationId,
                                                       Long projectId,
                                                       ExcelImportTemplate.Cursor cursor,
                                                       List<String> fieldCodes) {
        int col = fieldCodes.indexOf(FieldCode.PRODUCT);
        if (col == -1) {
            return null;
        }
        List<ProductVO> productVOList  = new ArrayList<>();
        if (agilePluginService != null) {
            productVOList = agilePluginService.listProductByProjectId(organizationId, projectId);
        }
        List<String> values = new ArrayList<>();
        productVOList.forEach(i -> values.add(i.getName()));
        return new PredefinedDTO(values,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.PRODUCT,
                cursor.getAndIncreaseSheetNum());
    }

    @Override
    public int getColByFieldCode(List<String> fieldCodes, String fieldCode) {
        int col = fieldCodes.indexOf(fieldCode);
        if (col == -1) {
            String msg = "error.fieldCodes." + fieldCode + ".not.exist";
            throw new CommonException(msg);
        }
        return col;
    }

    @Override
    public Double getProcess(Integer currentNum, Integer totalNum) {
        double process = (currentNum + 1.0) / (totalNum + 1.0) * 0.95 * 100;
        BigDecimal b = BigDecimal.valueOf(process);
        process = b.setScale(1, RoundingMode.HALF_UP).doubleValue();
        return process;
    }

    @Override
    public void copyGuideSheetFromTemplate(Workbook wb, String path) {
        Sheet guideSheet = wb.createSheet("要求");
        InputStream inputStream = this.getClass().getResourceAsStream(path);
        XSSFWorkbook srcWorkbook = null;
        try {
            srcWorkbook = new XSSFWorkbook(inputStream);
        } catch (IOException e) {
            throw new CommonException("error.open.issue.guide.template");
        }
        ExcelUtil.copySheet(srcWorkbook.getSheetAt(0), guideSheet, ExcelUtil.copyCellStyle(srcWorkbook, wb));
    }

    @Override
    public List<PredefinedDTO> processCustomFieldPredefinedList(Long projectId,
                                                                List<String> customFields,
                                                                ExcelImportTemplate.Cursor cursor,
                                                                int systemFieldLength,
                                                                Map<String, String> customFieldCodeNameMap,
                                                                String issueTypeList) {
        List<PredefinedDTO> result = new ArrayList<>();
        if (ObjectUtils.isEmpty(customFields)) {
            return result;
        }
        List<ObjectSchemeFieldDetailVO> objectSchemeFieldDetails =
                objectSchemeFieldService.queryCustomFieldList(projectId, issueTypeList);
        Map<String, List<String>> customFieldValueMap = new HashMap<>();
        List<String> customFieldCodes = new ArrayList<>();
        List<String> fieldTypes = Arrays.asList(FieldType.MULTIPLE, FieldType.SINGLE, FieldType.CHECKBOX, FieldType.RADIO);
        List<String> userNames =
                baseFeignClient.listUsersByProjectId(projectId, 1, 0, null)
                        .getBody()
                        .getContent()
                        .stream()
                        .map(UserDTO::getRealName)
                        .collect(Collectors.toList());
        objectSchemeFieldDetails.forEach(o -> {
            String fieldCode = o.getCode();
            String fieldName = o.getName();
            customFieldCodeNameMap.put(fieldCode, fieldName);
            customFieldCodes.add(fieldCode);
            String fieldType = o.getFieldType();
            if (fieldTypes.contains(fieldType)) {
                List<String> optionValues = o.getFieldOptions().stream().map(FieldOptionVO::getValue).collect(Collectors.toList());
                customFieldValueMap.put(fieldCode, optionValues);
            }
            if ("member".equals(fieldType) || FieldType.MULTI_MEMBER.equals(fieldType)) {
                customFieldValueMap.put(fieldCode, userNames);
            }
        });
        isCustomFieldsIllegal(customFields, customFieldCodes);
        for (int i = 0; i < customFields.size(); i++) {
            String code = customFields.get(i);
            List<String> values = customFieldValueMap.get(code);
            if (!ObjectUtils.isEmpty(values)) {
                result.add(
                        new PredefinedDTO(
                                values,
                                PREDEFINED_VALUE_START_ROW,
                                PREDEFINED_VALUE_END_ROW,
                                i + systemFieldLength,
                                i + systemFieldLength,
                                code,
                                cursor.getAndIncreaseSheetNum()));
            }
        }
        return result;
    }

    private void isCustomFieldsIllegal(List<String> customFields, List<String> customFieldCodes) {
        customFields.forEach(c -> {
            if (!customFieldCodes.contains(c)) {
                throw new CommonException("error.illegal.custom.field.code."+ c);
            }
        });
    }

    @Override
    public void fillInPredefinedValues(Workbook wb, Sheet sheet, List<PredefinedDTO> predefinedList) {
        for (PredefinedDTO predefined : predefinedList) {
            //父级保持issueId倒序
            if (!Objects.equals(ExcelImportTemplate.IssueHeader.PARENT, predefined.hidden())) {
                Collections.sort(predefined.values());
            }
            wb = ExcelUtil
                    .dropDownList2007(
                            wb,
                            sheet,
                            predefined.values(),
                            predefined.startRow(),
                            predefined.endRow(),
                            predefined.startCol(),
                            predefined.endCol(),
                            predefined.hidden(),
                            predefined.hiddenSheetIndex());
        }
    }

    @Override
    public Map<String, Long> getManagers(Long projectId) {
        Map<String, Long> managerMap = new HashMap<>();
        ResponseEntity<Page<UserDTO>> response = baseFeignClient.listUsersByProjectId(projectId, 1, 0, null);
        List<UserDTO> users = response.getBody().getContent();
        users.forEach(u -> {
            if (Boolean.TRUE.equals(u.getEnabled())) {
                String realName = u.getRealName();
                String loginName = u.getLoginName();
                Boolean isLdap = u.getLdap();
                String name;
                if (Boolean.TRUE.equals(isLdap)) {
                    name = realName + "（" + loginName + "）";
                } else {
                    name = realName + "（" + u.getEmail() + "）";
                }
                managerMap.put(name, u.getId());
            }
        });
        return managerMap;
    }
}
