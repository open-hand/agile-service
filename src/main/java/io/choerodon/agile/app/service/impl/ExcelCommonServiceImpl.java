package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.business.ProductVO;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.hzero.boot.file.FileClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/10
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ExcelCommonServiceImpl implements ExcelCommonService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ExcelCommonServiceImpl.class);

    private static final int PREDEFINED_VALUE_START_ROW = 1;
    private static final int PREDEFINED_VALUE_END_ROW = 500;
    private static final String VERSION_PLANNING = "version_planning";
    private static final String FIX_RELATION_TYPE = "fix";
    private static final String INFLUENCE_RELATION_TYPE = "influence";
    private static final String MULTIPART_NAME = "file";
    private static final String ORIGINAL_FILE_NAME = ".xlsx";
    private static final String FILE_NAME = "error.xlsx";
    private static final String ERROR_FILE_OPERATION_HISTORY_UPDATE = "error.FileOperationHistoryDTO.update";
    private static final String SUB_BUG_CN = "子缺陷";
    private static final String IMPORT_TEMPLATE_NAME = "导入模板";
    private static final String DATE_CHECK_MSG = "请输入正确的日期格式";
    private static final String DATE_RANGE_CHECK_MSG = "开始时间不能在结束时间之后";
    private static final String[] SYSTEM_DATE_FIELD_LIST = {FieldCode.ACTUAL_START_TIME, FieldCode.ACTUAL_END_TIME, FieldCode.ESTIMATED_START_TIME, FieldCode.ESTIMATED_END_TIME};
    private static final List<String> SYSTEM_FIELD_HEADER_NOT_SORT = Arrays.asList(FieldCode.PARENT, FieldCode.ISSUE_TYPE);
    public static final List<String> COMMON_PREDEFINED_SYSTEM_FIELD =
            Arrays.asList(
                    FieldCode.PRIORITY,
                    FieldCode.FIX_VERSION,
                    FieldCode.INFLUENCE_VERSION,
                    FieldCode.COMPONENT,
                    FieldCode.SPRINT,
                    FieldCode.ASSIGNEE,
                    FieldCode.REPORTER,
                    FieldCode.MAIN_RESPONSIBLE,
                    FieldCode.PARTICIPANT,
                    FieldCode.EPIC,
                    FieldCode.FEATURE,
                    FieldCode.LABEL,
                    FieldCode.ENVIRONMENT,
                    FieldCode.PRODUCT

            );

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
    @Autowired
    private FileOperationHistoryMapper fileOperationHistoryMapper;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueAssembler issueAssembler;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    @Autowired
    private FileClient fileClient;
    @Autowired
    private LookupValueService lookupValueService;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private MessageClientC7n messageClientC7n;
    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;

    @Override
    public PredefinedDTO processSystemFieldPredefined(Long projectId, ExcelImportTemplate.Cursor cursor, boolean withFeature, List<String> fieldCodes, String fieldCode) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        switch (fieldCode) {
            case FieldCode.PRIORITY:
                return processPriorityPredefined(organizationId, cursor, fieldCodes);
            case FieldCode.FIX_VERSION:
                return processVersionPredefined(projectId, cursor, fieldCodes);
            case FieldCode.INFLUENCE_VERSION:
                return processInfluenceVersionPredefined(projectId, cursor, fieldCodes);
            case FieldCode.COMPONENT:
                return processComponentPredefined(projectId, cursor, fieldCodes);
            case FieldCode.SPRINT:
                return processSprintPredefined(projectId, cursor, fieldCodes);
            case FieldCode.ASSIGNEE:
            case FieldCode.REPORTER:
            case FieldCode.MAIN_RESPONSIBLE:
            case FieldCode.PARTICIPANT:
                List<String> userNameList = new ArrayList<>(getManagers(projectId).keySet());
                return buildPredefinedByFieldCodeAndValues(cursor, fieldCodes, userNameList, fieldCode);
            case FieldCode.FEATURE:
                if (withFeature) {
                    return processEpicOrFeaturePredefined(organizationId, projectId, withFeature, cursor, fieldCodes);
                }
                break;
            case FieldCode.EPIC:
                if (!withFeature) {
                    return processEpicOrFeaturePredefined(organizationId, projectId, withFeature, cursor, fieldCodes);
                }
                break;
            case FieldCode.LABEL:
                return processLabelPredefined(projectId, cursor, fieldCodes);
            case FieldCode.ENVIRONMENT:
                return buildPredefinedByFieldCodeAndValues(cursor, fieldCodes, Arrays.asList("非生产环境", "生产环境"), FieldCode.ENVIRONMENT);
            case FieldCode.STATUS:
                return processIssueStatusPredefined(organizationId, projectId, cursor, fieldCodes);
            case FieldCode.PRODUCT:
                return processIssueProductPredefined(organizationId, projectId, cursor, fieldCodes);
            default:
                return null;
        }
        return null;
    }

    private PredefinedDTO processLabelPredefined(Long projectId,
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

    private PredefinedDTO processEpicOrFeaturePredefined(Long organizationId,
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

    private PredefinedDTO buildPredefinedByFieldCodeAndValues(ExcelImportTemplate.Cursor cursor,
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

    private PredefinedDTO processSprintPredefined(Long projectId,
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

    private PredefinedDTO processComponentPredefined(Long projectId,
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

    private PredefinedDTO processVersionPredefined(Long projectId,
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

    private PredefinedDTO processInfluenceVersionPredefined(Long projectId,
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

    private PredefinedDTO processPriorityPredefined(Long organizationId,
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

    private PredefinedDTO processIssueStatusPredefined(Long organizationId,
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

    private PredefinedDTO processIssueProductPredefined(Long organizationId,
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
                Optional.ofNullable(baseFeignClient.listUsersByProjectId(projectId, 1, 0, null)
                        .getBody()).orElse(new Page<>())
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
            if (!SYSTEM_FIELD_HEADER_NOT_SORT.contains(predefined.hidden())) {
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
        List<UserDTO> users = Optional.ofNullable(response.getBody()).orElse(new Page<>()).getContent();
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

    @Override
    public void validateWorkbook(Workbook workbook, FileOperationHistoryDTO history, String websocketKey) {
        int index = 1;
        if (workbook.getActiveSheetIndex() < 1
                || workbook.getSheetAt(index) == null
                || workbook.getSheetAt(index).getSheetName() == null
                || !IMPORT_TEMPLATE_NAME.equals(workbook.getSheetAt(index).getSheetName())) {
            history.setStatus("template_error");
            if (fileOperationHistoryMapper.updateByPrimaryKeySelective(history) != 1) {
                throw new CommonException(ERROR_FILE_OPERATION_HISTORY_UPDATE);
            }
            FileOperationHistoryDTO errorImport = fileOperationHistoryMapper.selectByPrimaryKey(history.getId());
            sendProcess(errorImport, history.getUserId(), 0.0, websocketKey);
            throw new CommonException("error.sheet.import");
        }
    }

    private void sendProcess(FileOperationHistoryDTO fileOperationHistoryDTO,
                             Long userId,
                             Double process,
                             String websocketKey) {
        fileOperationHistoryDTO.setProcess(process);
        String message = null;
        try {
            message = objectMapper.writeValueAsString(fileOperationHistoryDTO);
        } catch (JsonProcessingException e) {
            LOGGER.error("object to json error: {0}", e);
        }
        messageClientC7n.sendByUserId(userId, websocketKey, message);
    }

    @Override
    public List<String> resolveCodeFromHeader(Workbook workbook,
                                              FileOperationHistoryDTO history,
                                              String websocketKey) {
        Sheet dataSheet = workbook.getSheetAt(1);
        Row headerRow = dataSheet.getRow(0);
        if (headerRow == null) {
            history.setStatus("empty_data_sheet");
            fileOperationHistoryMapper.updateByPrimaryKeySelective(history);
            FileOperationHistoryDTO errorImport = fileOperationHistoryMapper.selectByPrimaryKey(history.getId());
            sendProcess(errorImport, history.getUserId(), 0.0, websocketKey);
            throw new CommonException("error.sheet.empty");
        }
        List<String> titles = new ArrayList<>();
        for (int i = 0; ; i++) {
            Cell cell = headerRow.getCell(i);
            if (isCellEmpty(cell)) {
                break;
            }
            titles.add(cell.toString());
        }
        return titles;
    }

    private boolean isCellEmpty(Cell cell) {
        return cell == null || cell.toString().equals("") || cell.getCellTypeEnum() == CellType.BLANK;
    }

    @Override
    public void addSystemFieldIfDateType(Set<Integer> dateTypeColumns,
                                         String code,
                                         int col,
                                         ExcelColumnVO excelColumnVO) {
        if (Arrays.asList(SYSTEM_DATE_FIELD_LIST).contains(code)) {
            dateTypeColumns.add(col);
            excelColumnVO.setDateType(true);
        }
    }

    @Override
    public void validateCustomField(Map<Integer, ExcelColumnVO> headerMap,
                                    Long projectId,
                                    FileOperationHistoryDTO history,
                                    String issueTypeList,
                                    Set<Integer> dateTypeColumns,
                                    String websocketKey) {
        List<ExcelColumnVO> customFields = new ArrayList<>();
        for (Map.Entry<Integer, ExcelColumnVO> entry : headerMap.entrySet()) {
            ExcelColumnVO value = entry.getValue();
            if (Boolean.TRUE.equals(value.isCustomField())) {
                customFields.add(value);
            }
        }
        List<ObjectSchemeFieldDetailVO> objectSchemeFieldDetails =
                objectSchemeFieldService.queryCustomFieldList(projectId, issueTypeList);
        List<UserDTO> users =
                baseFeignClient.listUsersByProjectId(projectId, 1, 0, null).getBody();
        List<String> userNames = new ArrayList<>();
        Map<String, Long> userMap = new HashMap<>();
        users.forEach(u -> {
            userNames.add(u.getRealName());
            userMap.put(u.getRealName(), u.getId());
        });

        Map<String, ObjectSchemeFieldDetailVO> fieldMap = new HashMap<>();
        objectSchemeFieldDetails.forEach(o -> fieldMap.put(o.getName(), o));
        StringBuilder status = new StringBuilder("error_custom_field_header_");
        List<String> multiValueFieldType = Arrays.asList("checkbox", "multiple","multiMember");
        List<String> fieldTypes = Arrays.asList("multiple", "single", "checkbox", "radio");
        List<String> dateTypes = Arrays.asList("date", "datetime", "time");
        for (ExcelColumnVO excelColumn : customFields) {
            String headerName = excelColumn.getFieldCode();
            ObjectSchemeFieldDetailVO detail = fieldMap.get(headerName);
            if (ObjectUtils.isEmpty(detail)) {
                status.append(headerName);
                history.setStatus(status.toString());
                fileOperationHistoryMapper.updateByPrimaryKeySelective(history);
                sendProcess(history, history.getUserId(), 0.0, websocketKey);
                throw new CommonException("error.illegal.custom.field.header." + headerName);
            } else {
                String fieldCode = detail.getCode();
                PageFieldViewUpdateVO fieldDetail = new PageFieldViewUpdateVO();
                fieldDetail.setFieldId(detail.getId());
                fieldDetail.setFieldType(detail.getFieldType());
                excelColumn.setCustomFieldDetail(fieldDetail);

                excelColumn.setFieldCode(fieldCode);
                String fieldType = detail.getFieldType();
                excelColumn.setMultiValue(multiValueFieldType.contains(fieldType));
                if (fieldTypes.contains(fieldType)) {
                    List<FieldOptionVO> fieldOptions = detail.getFieldOptions();
                    List<String> values = new ArrayList<>();
                    Map<String, Long> map = new HashMap<>();
                    fieldOptions.forEach(f -> {
                        values.add(f.getValue());
                        map.put(f.getValue(), f.getId());
                    });
                    excelColumn.setPredefinedValues(values);
                    excelColumn.setValueIdMap(map);
                }
                if ("member".equals(fieldType) || FieldType.MULTI_MEMBER.equals(fieldType)) {
                    excelColumn.setValueIdMap(userMap);
                    excelColumn.setPredefinedValues(userNames);
                }

                boolean isDateType = dateTypes.contains(fieldType);
                excelColumn.setDateType(isDateType);
                if (isDateType) {
                    dateTypeColumns.add(getColIndexByFieldCode(headerMap, fieldCode));
                }
            }
        }
    }

    private Integer getColIndexByFieldCode(Map<Integer, ExcelColumnVO> headerMap, String fieldCode) {
        for (Map.Entry<Integer, ExcelColumnVO> entry : headerMap.entrySet()) {
            if (entry.getValue().getFieldCode().equals(fieldCode)) {
                return entry.getKey();
            }
        }
        return null;
    }

    @Override
    public int processErrorData(Long userId,
                                FileOperationHistoryDTO history,
                                Sheet dataSheet,
                                Integer dataRowCount,
                                ExcelImportTemplate.Progress progress,
                                Map<Integer, List<Integer>> errorRowColMap,
                                int rowNum,
                                Set<Integer> sonSet,
                                int parentColIndex, int lastSendCountNum,
                                String websocketKey) {
        setErrorMsgToParentSonRow(rowNum, dataSheet, errorRowColMap, sonSet, parentColIndex);
        int errorCount = sonSet.size() + 1;
        Long failCount = progress.getFailCount() + errorCount;
        history.setFailCount(failCount);
        int processNum = progress.getProcessNum() + errorCount;
        progress.setFailCount(failCount);
        progress.addProcessNum(errorCount);
        if ((processNum - lastSendCountNum) * 1.0 / dataRowCount >= 0.1) {
            sendProcess(history, userId, processNum * 1.0 / dataRowCount, websocketKey);
            lastSendCountNum = processNum;
        }
        return lastSendCountNum;
    }

    private void setErrorMsgToParentSonRow(int rowNum,
                                           Sheet dataSheet,
                                           Map<Integer, List<Integer>> errorRowColMap,
                                           Set<Integer> sonSet,
                                           int parentColIndex) {
        addErrorMsgIfNotExisted(rowNum, dataSheet, errorRowColMap, parentColIndex);
        sonSet.forEach(s -> addErrorMsgIfNotExisted(s, dataSheet, errorRowColMap, parentColIndex));
    }

    private void addErrorMsgIfNotExisted(int rowNum,
                                         Sheet dataSheet,
                                         Map<Integer, List<Integer>> errorRowColMap,
                                         int parentColIndex) {
        if (ObjectUtils.isEmpty(errorRowColMap.get(rowNum))) {
            errorRowColMap.put(rowNum, Arrays.asList(parentColIndex));
            Row row = dataSheet.getRow(rowNum);
            Cell cell = row.getCell(parentColIndex);
            if (isCellEmpty(cell)) {
                cell = row.createCell(parentColIndex);
            }
            String value = cell.toString();
            cell.setCellValue(buildWithErrorMsg(value, "父子结构中有错误数据或父子结构插入错误"));
        }
    }

    private String buildWithErrorMsg(String value, String msg) {
        return new StringBuilder(value).append("(").append(msg).append(")").toString();
    }

    @Override
    public void insertCustomFields(Long issueId,
                                   List<PageFieldViewUpdateVO> customFields,
                                   Long projectId) {
        if (!ObjectUtils.isEmpty(customFields)) {
            BatchUpdateFieldsValueVo batchUpdateFieldsValueVo = new BatchUpdateFieldsValueVo();
            batchUpdateFieldsValueVo.setCustomFields(customFields);
            batchUpdateFieldsValueVo.setIssueIds(Collections.singletonList(issueId));
            batchUpdateFieldsValueVo.setPredefinedFields(new JSONObject());
            fieldValueService.handlerCustomFields(projectId, customFields, "agile_issue", batchUpdateFieldsValueVo.getIssueIds(), null, false, new HashMap<>());
            //导入创建问题通知自定义字段人员
            IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
            IssueVO result = issueAssembler.issueDetailDTOToVO(issue, new HashMap<>(), new HashMap<>(), new HashMap<>());
            sendMsgUtil.sendMsgToCustomFieldUsersByIssueCreate(projectId, result, DetailsHelper.getUserDetails().getUserId());
        }
    }

    @Override
    public void generateErrorDataExcelAndUpload(Map<Integer, List<Integer>> errorRowColMap,
                                                Sheet dataSheet,
                                                Map<Integer, ExcelColumnVO> headerMap,
                                                List<String> headerNames,
                                                FileOperationHistoryDTO history,
                                                Long organizationId,
                                                String templatePath) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        copyGuideSheetFromTemplate(workbook, templatePath);
        Sheet sheet = workbook.createSheet(IMPORT_TEMPLATE_NAME);
        CellStyle style = CatalogExcelUtil.getHeadStyle(workbook);
        ExcelUtil.generateHeaders(sheet, style, headerNames);
        List<PredefinedDTO> predefinedList = processPredefinedByHeaderMap(headerMap);
        fillInPredefinedValues(workbook, sheet, predefinedList);
        int colNum = headerNames.size() + 1;
        writeErrorData(errorRowColMap, dataSheet, colNum, sheet, headerMap);
        String errorWorkBookUrl = uploadErrorExcel(workbook, organizationId);
        history.setFileUrl(errorWorkBookUrl);
    }

    private void writeErrorData(Map<Integer, List<Integer>> errorRowColMap,
                                Sheet dataSheet,
                                int colNum,
                                Sheet sheet,
                                Map<Integer, ExcelColumnVO> headerMap) {
        XSSFWorkbook workbook = (XSSFWorkbook)sheet.getWorkbook();
        XSSFCellStyle ztStyle = workbook.createCellStyle();
        Font ztFont = workbook.createFont();
        ztFont.setColor(Font.COLOR_RED);
        ztStyle.setFont(ztFont);
        int startRow = 1;
        List<Integer> errorRows = new ArrayList<>(errorRowColMap.keySet());
        Collections.sort(errorRows);
        for (Integer rowNum : errorRows) {
            List<Integer> errorCol = errorRowColMap.get(rowNum);
            Row originRow = dataSheet.getRow(rowNum);
            Row row = sheet.createRow(startRow);
            for (int i = 0; i < colNum; i++) {
                Cell originCell = originRow.getCell(i);
                if (!isCellEmpty(originCell)) {
                    ExcelColumnVO excelColumnVO = headerMap.get(i);
                    Cell cell = row.createCell(i);
                    CellStyle cellStyle = workbook.createCellStyle();
                    cellStyle.cloneStyleFrom(originCell.getCellStyle());
                    cell.setCellStyle(cellStyle);
                    if (!errorCol.contains(i) && excelColumnVO.isDateType()){
                        if (originCell.getCellTypeEnum().equals(CellType.NUMERIC)){
                            cell.setCellValue(originCell.getDateCellValue());
                        } else {
                            cell.setCellValue(originCell.getStringCellValue());
                        }
                    } else {
                        cell.setCellValue(ExcelUtil.substring(originCell.toString()));
                    }
                    if (errorCol.contains(i)) {
                        cell.setCellStyle(ztStyle);
                    }
                }
            }
            startRow++;
        }
    }

    private List<PredefinedDTO> processPredefinedByHeaderMap(Map<Integer, ExcelColumnVO> headerMap) {
        List<PredefinedDTO> result = new ArrayList<>();
        ExcelImportTemplate.Cursor cursor = new ExcelImportTemplate.Cursor();
        headerMap.forEach((k, v) -> {
            int col = k;
            ExcelColumnVO excelColumn = v;
            List<String> values = excelColumn.getPredefinedValues();
            if (!ObjectUtils.isEmpty(values)) {
                PredefinedDTO dto =
                        new PredefinedDTO(values,
                                PREDEFINED_VALUE_START_ROW,
                                PREDEFINED_VALUE_END_ROW,
                                col,
                                col,
                                excelColumn.getFieldCode(),
                                cursor.getAndIncreaseSheetNum());
                result.add(dto);
            }
        });
        return result;
    }

    private String uploadErrorExcel(Workbook errorWorkbook, Long organizationId) {
        // 上传错误的excel
        MultipartFile multipartFile = new MultipartExcelUtil(MULTIPART_NAME, ORIGINAL_FILE_NAME, errorWorkbook);
        return fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), null, FILE_NAME, multipartFile);
    }

    @Override
    public void updateFinalRecode(FileOperationHistoryDTO fileOperationHistoryDTO,
                                  Long successCount,
                                  Long failCount,
                                  String status,
                                  String websocketKey) {
        FileOperationHistoryDTO update = new FileOperationHistoryDTO();
        update.setId(fileOperationHistoryDTO.getId());
        update.setSuccessCount(successCount);
        update.setFailCount(failCount);
        update.setStatus(status);
        update.setFileUrl(fileOperationHistoryDTO.getFileUrl());
        update.setObjectVersionNumber(fileOperationHistoryDTO.getObjectVersionNumber());
        if (fileOperationHistoryMapper.updateByPrimaryKeySelective(update) != 1) {
            throw new CommonException(ERROR_FILE_OPERATION_HISTORY_UPDATE);
        }
        FileOperationHistoryDTO result = fileOperationHistoryMapper.selectByPrimaryKey(update.getId());
        sendProcess(result, result.getUserId(), 1.0, websocketKey);
    }

    @Override
    public void setCommonSystemFieldPredefinedValueByCode(String code,
                                                          Long projectId,
                                                          Long organizationId,
                                                          ExcelColumnVO excelColumnVO,
                                                          boolean withFeature) {
        if (!COMMON_PREDEFINED_SYSTEM_FIELD.contains(code)) {
            return;
        }
        switch (code) {
            case FieldCode.PRIORITY:
                processPriority(organizationId, excelColumnVO);
                break;
            case FieldCode.FIX_VERSION:
                processVersion(projectId, excelColumnVO);
                break;
            case FieldCode.INFLUENCE_VERSION:
                processVersion(projectId, excelColumnVO);
                break;
            case FieldCode.COMPONENT:
                processComponent(projectId, excelColumnVO);
                break;
            case FieldCode.SPRINT:
                processSprint(projectId, excelColumnVO);
                break;
            case FieldCode.ASSIGNEE:
            case FieldCode.REPORTER:
            case FieldCode.MAIN_RESPONSIBLE:
            case FieldCode.PARTICIPANT:
                processUser(projectId, excelColumnVO);
                break;
            case FieldCode.EPIC:
            case FieldCode.FEATURE:
                processEpicOrFeature(organizationId, projectId, withFeature, excelColumnVO);
                break;
            case FieldCode.LABEL:
                processLabel(projectId, excelColumnVO);
                break;
            case FieldCode.ENVIRONMENT:
                processEnvironment(excelColumnVO, projectId);
                break;
            case FieldCode.PRODUCT:
                processIssueProduct(projectId, excelColumnVO);
                break;
            default:
                break;
        }
    }

    private void processPriority(Long organizationId, ExcelColumnVO excelColumnVO) {
        List<PriorityVO> priorityVOList = priorityService.queryByOrganizationIdList(organizationId);
        List<String> priorityList =
                priorityVOList
                        .stream()
                        .filter(p -> Boolean.TRUE.equals(p.getEnable()))
                        .map(PriorityVO::getName)
                        .collect(Collectors.toList());
        excelColumnVO.setPredefinedValues(priorityList);
        Map<String, Long> map =
                priorityVOList.stream().collect(Collectors.toMap(PriorityVO::getName, PriorityVO::getId));
        excelColumnVO.setValueIdMap(map);
    }

    private void processVersion(Long projectId, ExcelColumnVO excelColumnVO) {
        List<ProductVersionCommonDTO> productVersionCommons = productVersionMapper.listByProjectId(projectId);
        Map<String, Long> map = new HashMap<>();
        List<String> values = new ArrayList<>();
        productVersionCommons.forEach(p -> {
            String statusCode = p.getStatusCode();
            if (VERSION_PLANNING.equals(statusCode)) {
                values.add(p.getName());
                map.put(p.getName(), p.getVersionId());
            }
        });
        excelColumnVO.setValueIdMap(map);
        excelColumnVO.setPredefinedValues(values);
    }

    private void processComponent(Long projectId, ExcelColumnVO excelColumnVO) {
        List<IssueComponentDTO> components = issueComponentMapper.selectByProjectId(projectId);
        Map<String, Long> map = new HashMap<>();
        List<String> values = new ArrayList<>();
        components.forEach(c -> {
            values.add(c.getName());
            map.put(c.getName(), c.getComponentId());
        });
        excelColumnVO.setPredefinedValues(values);
        excelColumnVO.setValueIdMap(map);
    }

    private void processSprint(Long projectId, ExcelColumnVO excelColumnVO) {
        List<SprintDTO> sprints = sprintMapper.selectNotDoneByProjectId(projectId);
        Map<String, Long> map = new HashMap<>();
        List<String> values = new ArrayList<>();
        sprints.forEach(s -> {
            values.add(s.getSprintName());
            map.put(s.getSprintName(), s.getSprintId());
        });
        excelColumnVO.setValueIdMap(map);
        excelColumnVO.setPredefinedValues(values);
    }

    private void processUser(Long projectId, ExcelColumnVO excelColumnVO) {
        Map<String, Long> map = getManagers(projectId);
        List<String> values = new ArrayList<>(map.keySet());
        excelColumnVO.setPredefinedValues(values);
        excelColumnVO.setValueIdMap(map);
    }

    private void processEpicOrFeature(Long organizationId,
                                      Long projectId,
                                      boolean withFeature,
                                      ExcelColumnVO excelColumnVO) {
        List<String> values = new ArrayList<>();
        Map<String, Long> map = new HashMap<>();
        if (withFeature && agilePluginService != null) {
            List<SubFeatureVO> features = agilePluginService.listFeature(organizationId, projectId);
            features.forEach(f -> {
                values.add(f.getSummary());
                map.put(f.getSummary(), f.getIssueId());
            });
        } else {
            map.putAll(getEpicMap(projectId));
            values.addAll(map.keySet());
        }
        excelColumnVO.setPredefinedValues(values);
        excelColumnVO.setValueIdMap(map);
    }

    private void processLabel(Long projectId, ExcelColumnVO excelColumnVO) {
        List<IssueLabelDTO> labels =
                issueLabelMapper.selectByProjectIds(Arrays.asList(projectId));
        List<String> values = new ArrayList<>();
        Map<String, Long> map = new HashMap<>();
        labels.forEach(l -> {
            values.add(l.getLabelName());
            map.put(l.getLabelName(), l.getLabelId());
        });
        excelColumnVO.setPredefinedValues(values);
        excelColumnVO.setValueIdMap(map);
    }

    private void processEnvironment(ExcelColumnVO excelColumnVO, Long projectId) {
        List<String> values = Arrays.asList("非生产环境", "生产环境");
        excelColumnVO.setPredefinedValues(values);
        LookupTypeWithValuesVO environment = lookupValueService.queryLookupValueByCode("environment", projectId);
        excelColumnVO.setEnvNameCodeMap(environment.getLookupValues().stream().collect(Collectors.toMap(LookupValueVO::getName, LookupValueVO::getValueCode)));
    }

    private void processIssueProduct(Long projectId, ExcelColumnVO excelColumnVO) {
        List<ProductVO> productVOList = new ArrayList<>();
        if (agilePluginService != null) {
            productVOList.addAll(agilePluginService.listProductByProjectId(ConvertUtil.getOrganizationId(projectId), projectId));
        }
        List<String> values = new ArrayList<>();
        Map<String, Long> map = new HashMap<>();
        if (!ObjectUtils.isEmpty(productVOList)) {
            productVOList.forEach(v -> {
                values.add(v.getName());
                map.put(v.getName(), v.getId());
            });
        }
        excelColumnVO.setPredefinedValues(values);
        excelColumnVO.setValueIdMap(map);
    }

    @Override
    public void validateCustomFieldData(Row row,
                                        Integer col,
                                        ExcelColumnVO excelColumn,
                                        Map<Integer, List<Integer>> errorRowColMap,
                                        IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        SimpleDateFormat formats = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        SimpleDateFormat formatTimeOnly = new SimpleDateFormat("HH:mm:ss");
        SimpleDateFormat formatYearOnly = new SimpleDateFormat("yyyy");
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            boolean multiValue = excelColumn.isMultiValue();
            Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
            List<String> valueList = new ArrayList<>();
            Object customFieldValue = null;
            if (multiValue) {
                valueList.addAll(splitByRegex(value));
            }
            List<String> values = excelColumn.getPredefinedValues();
            if (!ObjectUtils.isEmpty(values)) {
                if (multiValue) {
                    boolean ok = true;
                    List<String> ids = new ArrayList<>();
                    for (String str : valueList) {
                        if (!values.contains(str)) {
                            ok = false;
                            break;
                        } else {
                            ids.add(String.valueOf(valueIdMap.get(str)));
                        }
                    }
                    if (!ok) {
                        cell.setCellValue(buildWithErrorMsg(value, "自定义字段值错误"));
                        addErrorColumn(row.getRowNum(), col, errorRowColMap);
                    }
                    customFieldValue = ids;
                } else {
                    if (!values.contains(value)) {
                        cell.setCellValue(buildWithErrorMsg(value, "自定义字段值错误"));
                        addErrorColumn(row.getRowNum(), col, errorRowColMap);
                    } else {
                        customFieldValue = String.valueOf(valueIdMap.get(value));
                    }
                }
            } else {
                if (excelColumn.isDateType()) {
                    customFieldValue = parseDateToString(cell, row.getRowNum(), col, errorRowColMap, formats, formatTimeOnly, formatYearOnly);
                } else {
                    customFieldValue = value;
                }
            }
            PageFieldViewUpdateVO pageFieldViewUpdateVO = excelColumn.getCustomFieldDetail();
            List<PageFieldViewUpdateVO> customFields = issueCreateVO.getCustomFields();
            if (customFields == null) {
                customFields = new ArrayList<>();
                issueCreateVO.setCustomFields(customFields);
            }
            PageFieldViewUpdateVO pageFieldViewUpdate = new PageFieldViewUpdateVO();
            pageFieldViewUpdate.setFieldId(pageFieldViewUpdateVO.getFieldId());
            pageFieldViewUpdate.setFieldType(pageFieldViewUpdateVO.getFieldType());
            pageFieldViewUpdate.setValue(customFieldValue);
            customFields.add(pageFieldViewUpdate);
        }
    }

    /**
     * @see <a href="https://stackoverflow.com/questions/15710888/reading-time-values-from-spreadsheet-using-poi-api"></a>
     * @param cell
     * @param rowNum
     * @param col
     * @param errorRowColMap
     * @param format
     * @param formatTimeOnly
     * @param formatYearOnly
     * @return
     */
    private String parseDateToString(Cell cell,
                                     int rowNum,
                                     Integer col,
                                     Map<Integer, List<Integer>> errorRowColMap,
                                     SimpleDateFormat format,
                                     SimpleDateFormat formatTimeOnly,
                                     SimpleDateFormat formatYearOnly) {
        if (!cell.getCellTypeEnum().equals(CellType.NUMERIC)) {
            String value = cell.toString();
            cell.setCellValue(buildWithErrorMsg(value, DATE_CHECK_MSG));
            addErrorColumn(rowNum, col, errorRowColMap);
        } else {
            if (!DateUtil.isCellDateFormatted(cell)) {
                cell.setCellValue(buildWithErrorMsg(cell.toString(), DATE_CHECK_MSG));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                Date date = cell.getDateCellValue();
                String dateStamp = formatYearOnly.format(date);
                if (dateStamp.equals("1899")) {
                    //仅时间类型返回
                    String time = formatTimeOnly.format(date);
                    Date now = new Date();
                    return format.format(now).split(" ")[0] + " " + time;
                } else {
                    return format.format(date);
                }
            }
        }
        return null;
    }

    private void addErrorColumn(int rowNum, Integer col, Map<Integer, List<Integer>> errorRowColMap) {
        List<Integer> columns = errorRowColMap.computeIfAbsent(rowNum, k -> new ArrayList<>());
        columns.add(col);
    }

    private List<String> splitByRegex(String value) {
        String regex1 = ",";
        String regex2 = "，";
        List<String> result = new ArrayList<>();
        String[] array = value.split(regex1);
        for (String str : array) {
            result.addAll(Arrays.asList(str.split(regex2)));
        }
        return result;
    }

    @Override
    public void handlerRequireFiled(ExcelColumnVO excelColumn, Map<Long, List<String>> requireFieldMap, IssueCreateVO issueCreateVO, Long projectId){
        if ("issueType".equals(excelColumn.getFieldCode()) && !ObjectUtils.isEmpty(issueCreateVO.getIssueTypeId())) {
            List<String> list = requireFieldMap.get(issueCreateVO.getIssueTypeId());
            if (CollectionUtils.isEmpty(list)) {
                PageFieldViewParamVO pageFieldViewParamVO = new PageFieldViewParamVO();
                pageFieldViewParamVO.setPageCode("agile_issue_create");
                pageFieldViewParamVO.setSchemeCode("agile_issue");
                pageFieldViewParamVO.setIssueTypeId(issueCreateVO.getIssueTypeId());
                List<PageFieldViewVO> pageFieldViewVOS = pageFieldService.queryPageFieldViewList(ConvertUtil.getOrganizationId(projectId), projectId, pageFieldViewParamVO);
                List<String> fieldCodes = pageFieldViewVOS.stream().filter(v -> Boolean.TRUE.equals(v.getRequired())).map(PageFieldViewVO::getFieldCode).collect(Collectors.toList());
                requireFieldMap.put(issueCreateVO.getIssueTypeId(), fieldCodes);
            }
        }
    }

    @Override
    public Boolean checkRequireField(Map<Long, List<String>> requireFieldMap,
                                     ExcelColumnVO excelColum,
                                     IssueCreateVO issueCreateVO,
                                     Row row,
                                     Integer col,
                                     Map<Integer, List<Integer>> errorRowColMap) {
        Boolean checkRequireField = true;
        Cell cell = row.getCell(col);
        if (isCellEmpty(cell)) {
            List<String> list = requireFieldMap.get(issueCreateVO.getIssueTypeId());
            if (!CollectionUtils.isEmpty(list) && list.contains(excelColum.getFieldCode())) {
                cell = row.createCell(col);
                cell.setCellValue("必填字段不能为空");
                addErrorColumn(row.getRowNum(), col, errorRowColMap);
                checkRequireField = false;
            }
        }
        return checkRequireField;
    }

    @Override
    public void validateCommonSystemFieldData(Row row,
                                              Integer col,
                                              ExcelColumnVO excelColumn,
                                              Map<Integer, List<Integer>> errorRowColMap,
                                              IssueCreateVO issueCreateVO,
                                              IssueVO parentIssue,
                                              Long projectId,
                                              Map<Integer, ExcelColumnVO> headerMap) {
        String fieldCode = excelColumn.getFieldCode();
        int issueTypeCol = getColIndexByFieldCode(headerMap, FieldCode.ISSUE_TYPE);
        String issueType = row.getCell(issueTypeCol).toString();
        String issueTypeCode = getIssueTypeCode(headerMap, issueType);
        switch (fieldCode) {
            case FieldCode.ISSUE_TYPE:
                validateAndSetIssueType(row, col, excelColumn, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.ASSIGNEE:
                validateAndSetAssignee(row, col, excelColumn, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.REPORTER:
                validateAndSetReporter(row, col, excelColumn, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.PRIORITY:
                validateAndSetPriority(row, col, excelColumn, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.REMAINING_TIME:
                validateAndSetRemainingTime(row, col, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.FIX_VERSION:
                validateAndSetFixVersion(row, col, excelColumn, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.INFLUENCE_VERSION:
                validateAndSetInfluenceVersion(row, col, excelColumn, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.STORY_POINTS:
                validateAndSetStoryPoint(row, col, errorRowColMap, issueCreateVO, issueTypeCode);
                break;
            case FieldCode.EPIC_NAME:
                validateAndSetEpicName(row, col, errorRowColMap, issueCreateVO, issueTypeCode, projectId, headerMap);
                break;
            case FieldCode.FEATURE:
                validateAndSetFeature(row, col, excelColumn, errorRowColMap, issueCreateVO, issueTypeCode, issueType);
                break;
            case FieldCode.EPIC:
                validateAndSetEpic(row, col, excelColumn, errorRowColMap, issueCreateVO, issueTypeCode, parentIssue, issueType);
                break;
            case FieldCode.SUMMARY:
                validateAndSetSummary(row, col, errorRowColMap, issueCreateVO);
                break;
            case ExcelImportTemplate.IssueHeader.PARENT:
                setParent(row, col, issueCreateVO, errorRowColMap, parentIssue, issueType, issueTypeCode);
                break;
            case FieldCode.DESCRIPTION:
                setDescription(row, col, issueCreateVO);
                break;
            case FieldCode.COMPONENT:
                validateAndSetComponent(row, col, excelColumn, parentIssue, issueType, issueTypeCode, issueCreateVO, errorRowColMap);
                break;
            case FieldCode.SPRINT:
                validateAndSetSprint(row, col, excelColumn, parentIssue, issueType, issueTypeCode, issueCreateVO, errorRowColMap);
                break;
            case FieldCode.LABEL:
                validateAndSetLabel(row, col, excelColumn, issueCreateVO, errorRowColMap, projectId);
                break;
            case FieldCode.ESTIMATED_START_TIME:
                validateAndSetEstimatedTime(row, col, issueCreateVO, errorRowColMap, FieldCode.ESTIMATED_START_TIME, headerMap);
                break;
            case FieldCode.ESTIMATED_END_TIME:
                validateAndSetEstimatedTime(row, col, issueCreateVO, errorRowColMap, FieldCode.ESTIMATED_END_TIME, headerMap);
                break;
            case ExcelImportTemplate.IssueHeader.RELATE_ISSUE:
                validateRelateIssue(row, col, issueCreateVO, errorRowColMap, projectId);
                break;
            case FieldCode.MAIN_RESPONSIBLE:
                validateAndSetMainResponsible(row, col, issueCreateVO, errorRowColMap, excelColumn, issueTypeCode);
                break;
            case FieldCode.ENVIRONMENT:
                validateAndSetEnvironment(row, col, issueCreateVO, errorRowColMap, excelColumn, issueTypeCode);
                break;
            case FieldCode.ISSUE_STATUS:
                validateAndSetIssueStatus(row, col, excelColumn, errorRowColMap, issueCreateVO, issueType);
                break;
            case FieldCode.ACTUAL_START_TIME:
                validateAndSetActualTime(row, col, issueCreateVO, errorRowColMap, FieldCode.ACTUAL_START_TIME, headerMap);
                break;
            case FieldCode.ACTUAL_END_TIME:
                validateAndSetActualTime(row, col, issueCreateVO, errorRowColMap, FieldCode.ACTUAL_END_TIME, headerMap);
                break;
            case FieldCode.PARTICIPANT:
                validateAndSetParticipant(row, col, excelColumn, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.ESTIMATE_TIME:
                validateAndSetEstimateTime(row, col, errorRowColMap, issueCreateVO);
                break;
            case FieldCode.PRODUCT:
                validateAndSetProduct(row, col, excelColumn, errorRowColMap, issueCreateVO);
                break;
            default:
                break;
        }
    }

    private String getIssueTypeCode(Map<Integer, ExcelColumnVO> headerMap,
                                    String issueType) {
        if (!ObjectUtils.isEmpty(headerMap)) {
            for (Map.Entry<Integer, ExcelColumnVO> entry : headerMap.entrySet()) {
                if (FieldCode.ISSUE_TYPE.equals(entry.getValue().getFieldCode())) {
                    Map<String, IssueTypeVO> issueTypeMap = entry.getValue().getIssueTypeMap();
                    IssueTypeVO vo = issueTypeMap.get(issueType);
                    if (vo != null) {
                        return vo.getTypeCode();
                    }
                }
            }
        }
        return null;
    }

    private void validateAndSetIssueType(Row row,
                                           Integer col,
                                           ExcelColumnVO excelColumn,
                                           Map<Integer, List<Integer>> errorRowColMap,
                                           IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        Integer rowNum = row.getRowNum();
        String value = cell.toString();
        Map<String, IssueTypeVO> issueTypeMap = excelColumn.getIssueTypeMap();
        List<String> values = excelColumn.getPredefinedValues();
        if (!values.contains(value)){
            cell.setCellValue(buildWithErrorMsg(value, IssueConstant.ISSUE_TYPE_CN + "错误"));
            addErrorColumn(rowNum, col, errorRowColMap);
        } else {
            IssueTypeVO issueTypeVO = issueTypeMap.get(value);
            issueCreateVO.setIssueTypeId(issueTypeVO.getId());
            issueCreateVO.setTypeCode(issueTypeVO.getTypeCode());
        }
    }

    private void validateAndSetAssignee(Row row,
                                        Integer col,
                                        ExcelColumnVO excelColumn,
                                        Map<Integer, List<Integer>> errorRowColMap,
                                        IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            List<String> values = excelColumn.getPredefinedValues();
            Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
            if (!values.contains(value)) {
                cell.setCellValue(buildWithErrorMsg(value, "经办人输入错误"));
                addErrorColumn(row.getRowNum(), col, errorRowColMap);
            } else {
                issueCreateVO.setAssigneeId(valueIdMap.get(value));
            }
        }
    }

    private void validateAndSetReporter(Row row,
                                        Integer col,
                                        ExcelColumnVO excelColumn,
                                        Map<Integer, List<Integer>> errorRowColMap,
                                        IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            List<String> values = excelColumn.getPredefinedValues();
            Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
            if (!values.contains(value)) {
                cell.setCellValue(buildWithErrorMsg(value, "报告人输入错误"));
                addErrorColumn(row.getRowNum(), col, errorRowColMap);
            } else {
                issueCreateVO.setReporterId(valueIdMap.get(value));
            }
        }
    }

    private void validateAndSetPriority(Row row,
                                        Integer col,
                                        ExcelColumnVO excelColumn,
                                        Map<Integer, List<Integer>> errorRowColMap,
                                        IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        String value = "";
        if (isCellEmpty(cell)) {
            row.createCell(col).setCellValue(buildWithErrorMsg(value, "优先级不能为空"));
            addErrorColumn(row.getRowNum(), col, errorRowColMap);
        } else {
            value = cell.toString();
            List<String> values = excelColumn.getPredefinedValues();
            Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
            if (!values.contains(value)) {
                cell.setCellValue(buildWithErrorMsg(value, "优先级输入错误"));
                addErrorColumn(row.getRowNum(), col, errorRowColMap);
            } else {
                Long priorityId = valueIdMap.get(value);
                issueCreateVO.setPriorityCode("priority" + priorityId);
                issueCreateVO.setPriorityId(priorityId);
            }
        }
    }

    private void validateAndSetRemainingTime(Row row,
                                             Integer col,
                                             Map<Integer, List<Integer>> errorRowColMap,
                                             IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        Integer rowNum = row.getRowNum();
        if (!isCellEmpty(cell)) {
            String value = cell.toString().trim();
            validateBigDecimal(col, errorRowColMap, cell, rowNum, value);
            List<Integer> errorCol = errorRowColMap.get(rowNum);
            if (ObjectUtils.isEmpty(errorCol)) {
                issueCreateVO.setRemainingTime(new BigDecimal(value));
            }
        }
    }

    private void validateBigDecimal(Integer col,
                                    Map<Integer, List<Integer>> errorRowColMap,
                                    Cell cell,
                                    Integer rowNum,
                                    String value) {

        if (Boolean.FALSE.equals(NumberUtil.isNumeric(value))) {
            cell.setCellValue(buildWithErrorMsg(value, "请输入数字"));
            addErrorColumn(rowNum, col, errorRowColMap);
        } else if (getNumberOfIntegerPlace(value) > 3) {
            cell.setCellValue(buildWithErrorMsg(value, "最大支持3位整数"));
            addErrorColumn(rowNum, col, errorRowColMap);
        } else {
            if (NumberUtil.isInteger(value) || NumberUtil.canParseInteger(value)) {
                if (value.length() > 1 && "0".equals(value.substring(0, 0))) {
                    cell.setCellValue(buildWithErrorMsg(value, "请输入正确的数字"));
                    addErrorColumn(rowNum, col, errorRowColMap);
                }
            } else {
                BigDecimal values = new BigDecimal(value);
                if (getNumberOfDecimalPlaces(values) > 1) {
                    cell.setCellValue(buildWithErrorMsg(value, "小数点后只支持一位小数"));
                    addErrorColumn(rowNum, col, errorRowColMap);
                }
            }
        }
    }

    private int getNumberOfIntegerPlace(String value) {
        int index = value.indexOf('.');
        if (index < 0) {
            index = value.length();
        }
        return index;
    }

    private int getNumberOfDecimalPlaces(BigDecimal bigDecimal) {
        String string = bigDecimal.stripTrailingZeros().toPlainString();
        int index = string.indexOf('.');
        return index < 0 ? 0 : string.length() - index - 1;
    }

    private void validateAndSetFixVersion(Row row,
                                          Integer col,
                                          ExcelColumnVO excelColumn,
                                          Map<Integer, List<Integer>> errorRowColMap,
                                          IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            List<String> values = excelColumn.getPredefinedValues();
            Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
            if (!values.contains(value)) {
                cell.setCellValue(buildWithErrorMsg(value, "请输入正确的版本"));
                addErrorColumn(row.getRowNum(), col, errorRowColMap);
            } else {
                List<VersionIssueRelVO> versionIssueRelList = new ArrayList<>();
                VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
                versionIssueRelVO.setVersionId(valueIdMap.get(value));
                versionIssueRelVO.setRelationType(FIX_RELATION_TYPE);
                versionIssueRelList.add(versionIssueRelVO);
                issueCreateVO.setVersionIssueRelVOList(versionIssueRelList);
            }
        }
    }

    private void validateAndSetInfluenceVersion(Row row,
                                                Integer col,
                                                ExcelColumnVO excelColumn,
                                                Map<Integer, List<Integer>> errorRowColMap,
                                                IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        if (!Objects.equals("bug", issueCreateVO.getTypeCode())) {
            return;
        }
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            List<String> values = excelColumn.getPredefinedValues();
            Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
            if (!values.contains(value)) {
                cell.setCellValue(buildWithErrorMsg(value, "请输入正确的影响版本"));
                addErrorColumn(row.getRowNum(), col, errorRowColMap);
            } else {
                List<VersionIssueRelVO> versionIssueRelList = issueCreateVO.getVersionIssueRelVOList();
                if (CollectionUtils.isEmpty(versionIssueRelList)) {
                    versionIssueRelList = new ArrayList<>();
                }
                VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
                versionIssueRelVO.setVersionId(valueIdMap.get(value));
                versionIssueRelVO.setRelationType(INFLUENCE_RELATION_TYPE);
                versionIssueRelList.add(versionIssueRelVO);
                issueCreateVO.setVersionIssueRelVOList(versionIssueRelList);
            }
        }
    }

    private void validateAndSetStoryPoint(Row row,
                                          Integer col,
                                          Map<Integer, List<Integer>> errorRowColMap,
                                          IssueCreateVO issueCreateVO,
                                          String issueTypeCode) {
        if (IssueTypeCode.isStory(issueTypeCode)) {
            Cell cell = row.getCell(col);
            Integer rowNum = row.getRowNum();
            if (!isCellEmpty(cell)) {
                String value = cell.toString().trim();
                validateBigDecimal(col, errorRowColMap, cell, rowNum, value);
                List<Integer> errorCol = errorRowColMap.get(rowNum);
                if (ObjectUtils.isEmpty(errorCol)) {
                    issueCreateVO.setStoryPoints(new BigDecimal(value));
                }
            }
        }
    }

    private void validateAndSetEpicName(Row row,
                                        Integer col,
                                        Map<Integer, List<Integer>> errorRowColMap,
                                        IssueCreateVO issueCreateVO,
                                        String issueTypeCode,
                                        Long projectId,
                                        Map<Integer, ExcelColumnVO> headerMap) {
        if (IssueTypeCode.isEpic(issueTypeCode)) {
            int rowNum = row.getRowNum();
            Cell cell = row.getCell(col);
            String value = "";
            if (isCellEmpty(cell)) {
                row.createCell(col).setCellValue(buildWithErrorMsg(value, "史诗名称不能为空"));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                value = cell.toString().trim();
                if (value.length() > 20) {
                    cell.setCellValue(buildWithErrorMsg(value, "史诗名称过长，不能超过20位"));
                    addErrorColumn(rowNum, col, errorRowColMap);
                } else if (Boolean.FALSE.equals(checkEpicNameExist(projectId, value))) {
                    cell.setCellValue(buildWithErrorMsg(value, "史诗名称重复"));
                    addErrorColumn(rowNum, col, errorRowColMap);
                } else {
                    issueCreateVO.setEpicName(value);
                    issueCreateVO.setSummary(value);
                    resetEpicSummary(headerMap, value, row);
                }
            }
        }
    }

    private Boolean checkEpicNameExist(Long projectId, String epicName) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setEpicName(epicName);
        List<IssueDTO> issueDTOList = issueMapper.select(issueDTO);
        return issueDTOList == null || issueDTOList.isEmpty();
    }

    private void resetEpicSummary(Map<Integer, ExcelColumnVO> headerMap, String value, Row row) {
        for (Map.Entry<Integer, ExcelColumnVO> entry: headerMap.entrySet())  {
            ExcelColumnVO excelColumn = entry.getValue();
            String fieldCode = excelColumn.getFieldCode();
            if (FieldCode.SUMMARY.equals(fieldCode)) {
                int col = entry.getKey();
                row.getCell(col).setCellValue(value);
            }
        }
    }

    private void validateAndSetFeature(Row row,
                                       Integer col,
                                       ExcelColumnVO excelColumn,
                                       Map<Integer, List<Integer>> errorRowColMap,
                                       IssueCreateVO issueCreateVO,
                                       String issueTypeCode,
                                       String issueType) {
        if(IssueTypeCode.AGILE_PARENT_ISSUE_TYPES.contains(issueTypeCode) && !SUB_BUG_CN.equals(issueType)) {
            Cell cell = row.getCell(col);
            if (!isCellEmpty(cell)) {
                int rowNum = row.getRowNum();
                String value = cell.toString();
                List<String> values = excelColumn.getPredefinedValues();
                Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
                if (!values.contains(value)) {
                    cell.setCellValue(buildWithErrorMsg(value, "所属特性输入错误"));
                    addErrorColumn(rowNum, col, errorRowColMap);
                } else {
                    Long featureId = valueIdMap.get(value);
                    issueCreateVO.setFeatureId(featureId);
                    //如果特性关联史诗，也要设置史诗id
                    IssueDTO feature = issueMapper.selectByPrimaryKey(featureId);
                    if (feature != null && Objects.equals(0L, feature.getEpicId())) {
                        issueCreateVO.setEpicId(feature.getEpicId());
                    }
                }
            }

        }
    }

    private void validateAndSetEpic(Row row,
                                    Integer col,
                                    ExcelColumnVO excelColumn,
                                    Map<Integer, List<Integer>> errorRowColMap,
                                    IssueCreateVO issueCreateVO,
                                    String issueTypeCode,
                                    IssueVO parentIssue,
                                    String issueType) {

        if(IssueTypeCode.AGILE_PARENT_ISSUE_TYPES.contains(issueTypeCode)) {
            if (SUB_BUG_CN.equals(issueType) && parentIssue != null) {
                issueCreateVO.setEpicId(parentIssue.getEpicId());
            } else {
                Cell cell = row.getCell(col);
                if (!isCellEmpty(cell)) {
                    int rowNum = row.getRowNum();
                    String value = cell.toString();
                    List<String> values = excelColumn.getPredefinedValues();
                    Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
                    if (!values.contains(value)) {
                        cell.setCellValue(buildWithErrorMsg(value, "所属史诗输入错误"));
                        addErrorColumn(rowNum, col, errorRowColMap);
                    } else {
                        issueCreateVO.setEpicId(valueIdMap.get(value));
                    }
                }
            }

        }
    }

    private void validateAndSetSummary(Row row,
                                       Integer col,
                                       Map<Integer, List<Integer>> errorRowColMap,
                                       IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        String value = "";
        if (isCellEmpty(cell)) {
            row.createCell(col).setCellValue(buildWithErrorMsg(value,  "概要不能为空"));
            addErrorColumn(rowNum, col, errorRowColMap);
        } else {
            value = cell.toString();
            if (value.length() > 44) {
                cell.setCellValue(buildWithErrorMsg(value, "概要过长"));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                issueCreateVO.setSummary(value);
            }
        }
    }

    private void setParent(Row row,
                           Integer col,
                           IssueCreateVO issueCreateVO,
                           Map<Integer, List<Integer>> errorRowColMap,
                           IssueVO parentIssue,
                           String issueType,
                           String issueTypeCode) {
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        if(isCellEmpty(cell)) {
            cell = row.createCell(col);
        }
        String value = cell.toString();
        if (IssueTypeCode.isSubTask(issueTypeCode)) {
            Long parentId = parentIssue.getIssueId();
            issueCreateVO.setParentIssueId(parentId);
        } else if (SUB_BUG_CN.equals(issueType)) {
            if (parentIssue.getTypeCode().equals("bug")) {
                cell.setCellValue(buildWithErrorMsg(value, "子缺陷的父级不能为缺陷类型"));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                Long parentId = parentIssue.getIssueId();
                issueCreateVO.setRelateIssueId(parentId);
            }
        }
    }

    private void setDescription(Row row,
                                Integer col,
                                IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            issueCreateVO.setDescription("<p>"+ value + "</p>");
        }
    }

    private void validateAndSetComponent(Row row,
                                         Integer col,
                                         ExcelColumnVO excelColumn,
                                         IssueVO parentIssue,
                                         String issueType,
                                         String issueTypeCode,
                                         IssueCreateVO issueCreateVO,
                                         Map<Integer, List<Integer>> errorRowColMap) {
        if (SUB_BUG_CN.equals(issueType)
                || IssueTypeCode.isSubTask(issueTypeCode)) {
            List<ComponentIssueRelVO> components = parentIssue.getComponentIssueRelVOList();
            if (!ObjectUtils.isEmpty(components)) {
                issueCreateVO.setComponentIssueRelVOList(parentIssue.getComponentIssueRelVOList());
            }
        } else {
            Cell cell = row.getCell(col);
            int rowNum = row.getRowNum();
            if (!isCellEmpty(cell)) {
                String value = cell.toString();
                List<String> values = excelColumn.getPredefinedValues();
                Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
                if (!values.contains(value)) {
                    cell.setCellValue(buildWithErrorMsg(value, "请输入正确的模块"));
                    addErrorColumn(rowNum, col, errorRowColMap);
                } else {
                    ComponentIssueRelVO componentIssueRelVO = new ComponentIssueRelVO();
                    componentIssueRelVO.setComponentId(valueIdMap.get(value));
                    issueCreateVO.setComponentIssueRelVOList(Arrays.asList(componentIssueRelVO));
                }
            }
        }
    }

    private void validateAndSetSprint(Row row,
                                      Integer col,
                                      ExcelColumnVO excelColumn,
                                      IssueVO parentIssue,
                                      String issueType,
                                      String issueTypeCode,
                                      IssueCreateVO issueCreateVO,
                                      Map<Integer, List<Integer>> errorRowColMap) {
        if (SUB_BUG_CN.equals(issueType)
                || IssueTypeCode.isSubTask(issueTypeCode)) {
            Long sprintId = parentIssue.getSprintId();
            if (sprintId != null && !Objects.equals(0L, sprintId)) {
                issueCreateVO.setSprintId(sprintId);
            }
        } else if (IssueTypeCode.AGILE_PARENT_ISSUE_TYPES.contains(issueTypeCode)){
            Cell cell = row.getCell(col);
            int rowNum = row.getRowNum();
            if (!isCellEmpty(cell)) {
                String value = cell.toString();
                List<String> values = excelColumn.getPredefinedValues();
                Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
                if (!values.contains(value)) {
                    cell.setCellValue(buildWithErrorMsg(value, "请输入正确的冲刺"));
                    addErrorColumn(rowNum, col, errorRowColMap);
                } else {
                    issueCreateVO.setSprintId(valueIdMap.get(value));
                }
            }
        }
    }

    private void validateAndSetLabel(Row row,
                                     Integer col,
                                     ExcelColumnVO excelColumn,
                                     IssueCreateVO issueCreateVO,
                                     Map<Integer, List<Integer>> errorRowColMap,
                                     Long projectId) {
        Cell cell = row.getCell(col);
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            if (value.length() > 20) {
                cell.setCellValue(buildWithErrorMsg(value, "标签名称过长"));
                addErrorColumn(row.getRowNum(), col, errorRowColMap);
            } else {
                LabelIssueRelVO label = new LabelIssueRelVO();
                label.setProjectId(projectId);
                label.setLabelName(value);
                issueCreateVO.setLabelIssueRelVOList(Arrays.asList(label));
            }
        }
    }

    private void validateAndSetEstimatedTime(Row row,
                                             Integer col,
                                             IssueCreateVO issueCreateVO,
                                             Map<Integer, List<Integer>> errorRowColMap,
                                             String fieldCode,
                                             Map<Integer, ExcelColumnVO> headerMap) {
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        if (!isCellEmpty(cell)) {
            if (!cell.getCellTypeEnum().equals(CellType.NUMERIC)) {
                cell.setCellValue(buildWithErrorMsg("", DATE_CHECK_MSG));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                if (!DateUtil.isCellDateFormatted(cell)) {
                    cell.setCellValue(buildWithErrorMsg("", DATE_CHECK_MSG));
                    addErrorColumn(rowNum, col, errorRowColMap);
                } else {
                    Date date = cell.getDateCellValue();
                    boolean illegalDateRange = isStartDateAfterEndDate(date, row, fieldCode, headerMap);
                    if (illegalDateRange) {
                        cell.setCellValue(buildWithErrorMsg("", DATE_RANGE_CHECK_MSG));
                        addErrorColumn(rowNum, col, errorRowColMap);
                        return;
                    }
                    if (FieldCode.ESTIMATED_START_TIME.equals(fieldCode)) {
                        issueCreateVO.setEstimatedStartTime(date);
                    }
                    if (FieldCode.ESTIMATED_END_TIME.equals(fieldCode)) {
                        issueCreateVO.setEstimatedEndTime(date);
                    }
                }
            }
        }
    }

    private boolean isStartDateAfterEndDate(Date date,
                                            Row row,
                                            String fieldCode,
                                            Map<Integer, ExcelColumnVO> headerMap) {
        String anotherDateCode = null;
        switch (fieldCode) {
            case FieldCode.ESTIMATED_START_TIME:
                anotherDateCode = FieldCode.ESTIMATED_END_TIME;
                break;
            case FieldCode.ESTIMATED_END_TIME:
                anotherDateCode = FieldCode.ESTIMATED_START_TIME;
                break;
            case FieldCode.ACTUAL_START_TIME:
                anotherDateCode = FieldCode.ACTUAL_END_TIME;
                break;
            case FieldCode.ACTUAL_END_TIME:
                anotherDateCode = FieldCode.ACTUAL_START_TIME;
                break;
            default:
                break;
        }
        if (anotherDateCode == null) {
            return false;
        }
        Integer anotherDateCol = getColIndexByFieldCode(headerMap, anotherDateCode);
        if (anotherDateCol == null) {
            return false;
        }
        Cell anotherEsTimeCell = row.getCell(anotherDateCol);
        if (!isCellEmpty(anotherEsTimeCell)
                && anotherEsTimeCell.getCellTypeEnum().equals(CellType.NUMERIC)
                && DateUtil.isCellDateFormatted(anotherEsTimeCell)) {
            Date anotherDate = anotherEsTimeCell.getDateCellValue();
            Date startDate;
            Date endDate;
            if (FieldCode.ESTIMATED_START_TIME.equals(fieldCode)
                    || FieldCode.ACTUAL_START_TIME.equals(fieldCode)) {
                startDate = date;
                endDate = anotherDate;
            } else {
                startDate = anotherDate;
                endDate = date;
            }
            return !startDate.before(endDate);
        } else {
            return false;
        }
    }

    private void validateRelateIssue(Row row,
                                     Integer col,
                                     IssueCreateVO issueCreateVO,
                                     Map<Integer, List<Integer>> errorRowColMap,
                                     Long projectId) {
        String projectCode = projectInfoMapper.selectProjectCodeByProjectId(projectId);
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            String regex = "(([0-9]+(，|,))|((!|！)[0-9]+(，|,)))*(([0-9]+)|((!|！)[0-9]+))";
            if (Pattern.matches(regex, value)) {
                RelatedIssueVO relatedIssueVO = new RelatedIssueVO();
                issueCreateVO.setRelatedIssueVO(relatedIssueVO);
                relatedIssueVO.setRow(rowNum);
                Set<Long> relatedIssueIds = new HashSet<>();
                Set<Integer> relatedRows = new HashSet<>();
                List<String> values = splitByRegex(value);
                boolean ok = true;
                for (String str : values) {
                    if (str.startsWith("！") || str.startsWith("!")) {
                        relatedRows.add(Integer.valueOf(str.substring(1)) - 1);
                    } else {
                        int num = Integer.parseInt(str);
                        String issueNum = projectCode + "-" + num;
                        IssueVO issueVO = issueMapper.selectByIssueNum(projectId, issueNum);
                        if (issueVO == null) {
                            ok = false;
                            cell.setCellValue(buildWithErrorMsg(value, num + "不存在"));
                            addErrorColumn(rowNum, col, errorRowColMap);
                            break;
                        } else {
                            Boolean isSubTask = IssueTypeCode.isSubTask(issueVO.getTypeCode());
                            if (isSubTask){
                                break;
                            }
                            relatedIssueIds.add(issueVO.getIssueId());
                        }
                    }
                }
                if (ok) {
                    relatedIssueVO.setRelatedIds(relatedIssueIds);
                    relatedIssueVO.setRelatedRows(relatedRows);
                }
            } else {
                cell.setCellValue(buildWithErrorMsg(value, "关联" + IssueConstant.ISSUE_CN + "格式不正确"));
                addErrorColumn(rowNum, col, errorRowColMap);
            }
        }
    }

    private void validateAndSetMainResponsible(Row row,
                                               Integer col,
                                               IssueCreateVO issueCreateVO,
                                               Map<Integer, List<Integer>> errorRowColMap,
                                               ExcelColumnVO excelColumnVO,
                                               String issueTypeCode) {
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        if (IssueTypeCode.AGILE_ISSUE_TYPE_CODE_NO_EPIC.contains(issueTypeCode) && !isCellEmpty(cell)) {
            String value = cell.toString();
            List<String> values = excelColumnVO.getPredefinedValues();
            Map<String, Long> map = excelColumnVO.getValueIdMap();
            if (!values.contains(value)) {
                cell.setCellValue(buildWithErrorMsg(value, "请输入正确的主要负责人"));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                issueCreateVO.setMainResponsibleId(map.get(value));
            }
        }
    }

    private void validateAndSetEnvironment(Row row,
                                           Integer col,
                                           IssueCreateVO issueCreateVO,
                                           Map<Integer, List<Integer>> errorRowColMap,
                                           ExcelColumnVO excelColumnVO,
                                           String issueTypeCode) {
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        if (!isCellEmpty(cell) && IssueTypeCode.isBug(issueTypeCode)) {
            String value = cell.toString();
            List<String> values = excelColumnVO.getPredefinedValues();
            if (!values.contains(value)) {
                cell.setCellValue(buildWithErrorMsg(value, "请输入正确的环境"));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                Map<String, String> envNameCodeMap = excelColumnVO.getEnvNameCodeMap();
                issueCreateVO.setEnvironment(envNameCodeMap.getOrDefault(value, null));
            }
        }
    }

    private void validateAndSetIssueStatus(Row row,
                                           Integer col,
                                           ExcelColumnVO excelColumn,
                                           Map<Integer, List<Integer>> errorRowColMap,
                                           IssueCreateVO issueCreateVO,
                                           String issueType) {
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        Map<String, StatusVO> issueStatusMap = excelColumn.getIssueStatusMap();
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            StatusVO statusVO = issueStatusMap.get(issueType + "-" + value);
            if (statusVO == null) {
                cell.setCellValue(buildWithErrorMsg(value, "状态输入错误"));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                issueCreateVO.setStatusId(statusVO.getId());
            }
        }
    }

    private void validateAndSetActualTime(Row row,
                                          Integer col,
                                          IssueCreateVO issueCreateVO,
                                          Map<Integer, List<Integer>> errorRowColMap,
                                          String fieldCode,
                                          Map<Integer, ExcelColumnVO> headerMap) {
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        if (!isCellEmpty(cell)) {
            if (!cell.getCellTypeEnum().equals(CellType.NUMERIC)) {
                cell.setCellValue(buildWithErrorMsg("", DATE_CHECK_MSG));
                addErrorColumn(rowNum, col, errorRowColMap);
            } else {
                if (!DateUtil.isCellDateFormatted(cell)) {
                    cell.setCellValue(buildWithErrorMsg("", DATE_CHECK_MSG));
                    addErrorColumn(rowNum, col, errorRowColMap);
                } else {
                    Date date = cell.getDateCellValue();
                    boolean illegalDateRange = isStartDateAfterEndDate(date, row, fieldCode, headerMap);
                    if (illegalDateRange) {
                        cell.setCellValue(buildWithErrorMsg("", DATE_RANGE_CHECK_MSG));
                        addErrorColumn(rowNum, col, errorRowColMap);
                        return;
                    }
                    if (FieldCode.ACTUAL_START_TIME.equals(fieldCode)) {
                        issueCreateVO.setActualStartTime(date);
                    }
                    if (FieldCode.ACTUAL_END_TIME.equals(fieldCode)) {
                        issueCreateVO.setActualEndTime(date);
                    }
                }
            }
        }
    }

    private void validateAndSetParticipant(Row row,
                                           Integer col,
                                           ExcelColumnVO excelColumn,
                                           Map<Integer, List<Integer>> errorRowColMap,
                                           IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            List<Long> participantIds = new ArrayList<>();
            List<String> list = splitByRegex(value);
            for (String participant : list) {
                List<String> values = excelColumn.getPredefinedValues();
                Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
                if (!values.contains(participant)) {
                    cell.setCellValue(buildWithErrorMsg(participant, "请输入正确的用户"));
                    addErrorColumn(row.getRowNum(), col, errorRowColMap);
                    break;
                } else {
                    participantIds.add(valueIdMap.get(participant));
                }
            }
            issueCreateVO.setParticipantIds(participantIds);
        }
    }

    private void validateAndSetEstimateTime(Row row,
                                            Integer col,
                                            Map<Integer, List<Integer>> errorRowColMap,
                                            IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        Integer rowNum = row.getRowNum();
        if (!isCellEmpty(cell)) {
            String value = cell.toString().trim();
            validateBigDecimal(col, errorRowColMap, cell, rowNum, value);
            List<Integer> errorCol = errorRowColMap.get(rowNum);
            if (ObjectUtils.isEmpty(errorCol)) {
                issueCreateVO.setEstimateTime(new BigDecimal(value));
            }
        }
    }

    private void validateAndSetProduct(Row row,
                                       Integer col,
                                       ExcelColumnVO excelColumn,
                                       Map<Integer, List<Integer>> errorRowColMap,
                                       IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            List<Long> productIds = new ArrayList<>();
            List<String> list = splitByRegex(value);
            for (String product : list) {
                List<String> values = excelColumn.getPredefinedValues();
                Map<String, Long> valueIdMap = excelColumn.getValueIdMap();
                if (!values.contains(product)) {
                    cell.setCellValue(buildWithErrorMsg(product, "请输入正确的产品"));
                    addErrorColumn(row.getRowNum(), col, errorRowColMap);
                    break;
                } else {
                    productIds.add(valueIdMap.get(product));
                }
            }
            issueCreateVO.setProductIds(productIds);
        }
    }

}