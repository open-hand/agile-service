package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.hzero.boot.file.FileClient;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.domain.Sort;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class ExcelServiceImpl implements ExcelService {

    protected static final Logger LOGGER = LoggerFactory.getLogger(ExcelServiceImpl.class);

    protected static final String SUB_TASK = "sub_task";
    private static final String ISSUE_EPIC = "issue_epic";
    protected static final String UPLOAD_FILE = "upload_file";
    protected static final String APPLY_TYPE_AGILE = "agile";
    protected static final String CANCELED = "canceled";
    protected static final String DOING = "doing";
    protected static final String SUCCESS = "success";
    protected static final String FAILED = "failed";
    protected static final String WEBSOCKET_IMPORT_CODE = "agile-import-issues";
    protected static final String FEATURE = "feature";
    protected static final String FILE_NAME = "error.xlsx";
    protected static final String MULTIPART_NAME = "file";
    protected static final String ORIGINAL_FILE_NAME = ".xlsx";

    private static final String FIELD_CODES = "fieldCodes";
    private static final String FIELD_NAMES = "fieldNames";
    private static final String WEBSOCKET_EXPORT_CODE = "agile-export-issue";
    private static final String EXCELCONTENTTYPE = "application/vnd.ms-excel";
    private static final String FILESUFFIX = ".xlsx";
    protected static final String DOWNLOAD_FILE = "download_file";
    protected static final String DOWNLOAD_FILE_PUBLISH_VERSION = "download_file_publish_version";
    private static final String EXPORT_ERROR_WORKBOOK_CLOSE = "error.issue.close.workbook";
    private static final String PROJECT_ERROR = "error.project.notFound";
    private static final String FIX_RELATION_TYPE = "fix";
    private static final String INFLUENCE_RELATION_TYPE = "influence";
    private static final String UPLOAD_FILE_CUSTOM_FIELD = "upload_file_customer_field";
    private static final String WEBSOCKET_IMPORT_CUSTOM_FIELD_CODE = "agile-import-customer-field-";
    protected static final String WEBSOCKET_EXPORT_PUBLISH_VERSION = "agile-export-publish-version";
    private static final String ERROR_FILE_OPERATION_HISTORY_UPDATE = "error.FileOperationHistoryDTO.update";

    protected static final String VERSION_PLANNING = "version_planning";

    protected static final String RELATION_TYPE_FIX = "fix";
    protected static final String IMPORT_TEMPLATE_NAME = "导入模板";
    private static final String DATE_CHECK_MSG = "请输入正确的日期格式";
    private static final String DATE_RANGE_CHECK_MSG = "开始时间不能在结束时间之后";

    protected static final String EPIC_CN = "史诗";
    protected static final String SUMMARY = "summary";
    protected static final String ISSUE_NUM = "issueNum";
    protected static final String EPIC_NAME = "epicName";
    protected static final String TYPE_NAME = "typeName";
    protected static final String DESCRIPTION = "description";
    protected static final String PRIORITY_NAME = "priorityName";
    protected static final String STATUS_NAME = "statusName";
    protected static final String RESOLUTION = "resolution";
    protected static final String SPRINT_NAME = "sprintName";
    protected static final String ASSIGNEE_NAME = "assigneeName";
    protected static final String REPORTER_NAME = "reporterName";
    protected static final String STORY_POINTS = "storyPoints";
    protected static final String REMAINING_TIME = "remainingTime";
    protected static final String ESTIMATE_TIME = "estimateTime";
    protected static final String VERSION_NAME = "versionName";
    protected static final String FIX_VERSION_NAME = "fixVersionName";
    protected static final String INFLUENCE_VERSION_NAME = "influenceVersionName";
    protected static final String LABEL_NAME = "labelName";
    protected static final String COMPONENT_NAME = "componentName";
    protected static final String CREATION_DATE = "creationDate";
    protected static final String LAST_UPDATE_DATE = "lastUpdateDate";
    protected static final String ESTIMATED_START_TIME = "estimatedStartTime";
    protected static final String ACTUAL_START_TIME = "actualStartTime";
    protected static final String ESTIMATED_END_TIME = "estimatedEndTime";
    protected static final String ACTUAL_END_TIME = "actualEndTime";
    protected static final String CREATED_USER_NAME = "createdUserName";
    protected static final String LAST_UPDATE_USER_NAME = "lastUpdatedUserName";
    protected static final String MAIN_RESPONSIBLE_NAME = "mainResponsibleName";
    protected static final String ENVIRONMENT_NAME = "environmentName";
    protected static final String SPENT_WORK_TIME = "spentWorkTime";
    protected static final String ALL_ESTIMATE_TIME = "allEstimateTime";
    protected static final String TAGS = "tags";
    protected static final String RELATED_ISSUE = "relatedIssue";
    protected static final String EPIC_SELF_NAME = "epicSelfName";
    protected static final String PARTICIPANT = "participant";
    protected static final String PRODUCT = "product";

    protected static final String USER_MAP = "userMap";
    protected static final String ISSUE_TYPE_MAP = "issueTypeMap";
    protected static final String STATUS_MAP = "statusMap";
    protected static final String PRIORITY_MAP = "priorityMap";
    protected static final String CLOSE_SPRINT_MAP = "closeSprintMap";
    protected static final String FIX_VERSION_MAP = "fixVersionMap";
    protected static final String INFLUENCE_VERSION_MAP = "influenceVersionMap";
    protected static final String LABEL_MAP = "labelMap";
    protected static final String COMPONENT_MAP = "componentMap";
    protected static final String FOUNDATION_CODE_VALUE_MAP = "foundationCodeValueMap";
    protected static final String ENV_MAP = "envMap";
    protected static final String WORK_LOG_MAP = "workLogMap";
    protected static final String TAG_MAP = "tagMap";
    protected static final String RELATED_ISSUE_MAP = "relatedIssueMap";
    protected static final String PRODUCT_MAP = "productMap";

    private static final String SUB_BUG_CN = "子缺陷";

    protected static final String COLON_CN = "：";

    private static final int PREDEFINED_VALUE_START_ROW = 1;
    private static final int PREDEFINED_VALUE_END_ROW = 500;
    private static final String INSERT = "insert";

    protected static final String[] SYSTEM_DATE_FIELD_LIST = {FieldCode.ACTUAL_START_TIME, FieldCode.ACTUAL_END_TIME, FieldCode.ESTIMATED_START_TIME, FieldCode.ESTIMATED_END_TIME};

    @Autowired
    protected StateMachineClientService stateMachineClientService;

    @Autowired
    private MessageClientC7n messageClientC7n;

    @Autowired
    private FileOperationHistoryMapper fileOperationHistoryMapper;

    @Autowired
    protected ProductVersionMapper productVersionMapper;

    @Autowired
    protected IssueService issueService;

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    protected IssueComponentMapper issueComponentMapper;

    @Autowired
    protected SprintMapper sprintMapper;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private PriorityService priorityService;

    @Autowired
    protected BaseFeignClient baseFeignClient;

    @Autowired
    private FileClient fileClient;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    protected ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private QuickFilterMapper quickFilterMapper;
    @Autowired
    protected ProjectInfoMapper projectInfoMapper;
    @Autowired
    private UserService userService;
    @Autowired
    protected PageFieldService pageFieldService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired
    private IssueLinkService issueLinkService;
    @Autowired
    private IssueLinkTypeMapper issueLinkTypeMapper;
    @Autowired
    private ObjectSchemeFieldExcelService objectSchemeFieldExcelService;
    @Autowired
    protected WorkLogMapper workLogMapper;
    @Autowired
    private StatusMapper statusMapper;
    @Autowired
    private LookupValueService lookupValueService;
    @Autowired
    protected IssueLinkMapper issueLinkMapper;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    @Autowired
    private IssueAssembler issueAssembler;

    private static final String[] FIELDS_NAMES;

    private static final String[] FIELDS;

    protected static Map<String, String> FIELD_MAP = new LinkedHashMap<>();

    protected static String[] AUTO_SIZE_WIDTH = {SUMMARY, EPIC_NAME, FEATURE,
            CREATION_DATE, LAST_UPDATE_DATE, SPRINT_NAME};

    static {
        FIELD_MAP.put(TYPE_NAME, IssueConstant.ISSUE_TYPE_CN);
        FIELD_MAP.put(ISSUE_NUM, IssueConstant.ISSUE_CN + "编号");
        FIELD_MAP.put(SUMMARY, "概要");
        FIELD_MAP.put(DESCRIPTION, "描述");
        FIELD_MAP.put(PRIORITY_NAME, "优先级");
        FIELD_MAP.put(STATUS_NAME, "状态");
        FIELD_MAP.put(RESOLUTION, "解决状态");
        FIELD_MAP.put(SPRINT_NAME, "冲刺");
        FIELD_MAP.put(ASSIGNEE_NAME, "经办人");
        FIELD_MAP.put(REPORTER_NAME, "报告人");
        FIELD_MAP.put(STORY_POINTS, "故事点");
        FIELD_MAP.put(REMAINING_TIME, "剩余预估时间");
        FIELD_MAP.put(ESTIMATE_TIME, "原始预估时间");
        FIELD_MAP.put(VERSION_NAME, "版本");
        FIELD_MAP.put(FIX_VERSION_NAME, "修复的版本");
        FIELD_MAP.put(INFLUENCE_VERSION_NAME, "影响的版本");
        FIELD_MAP.put(EPIC_NAME, "所属史诗");
        FIELD_MAP.put(LABEL_NAME, "标签");
        FIELD_MAP.put(COMPONENT_NAME, "模块");
        FIELD_MAP.put(CREATION_DATE, "创建时间");
        FIELD_MAP.put(LAST_UPDATE_DATE, "最后更新时间");
        FIELD_MAP.put(ESTIMATED_START_TIME, "预计开始时间");
        FIELD_MAP.put(ESTIMATED_END_TIME, "预计结束时间");
        FIELD_MAP.put(ACTUAL_START_TIME, "实际开始时间");
        FIELD_MAP.put(ACTUAL_END_TIME, "实际结束时间");
        FIELD_MAP.put(CREATED_USER_NAME, "创建人");
        FIELD_MAP.put(LAST_UPDATE_USER_NAME, "更新人");
        FIELD_MAP.put(MAIN_RESPONSIBLE_NAME, "主要负责人");
        FIELD_MAP.put(ENVIRONMENT_NAME, "环境");
        FIELD_MAP.put(SPENT_WORK_TIME, "已耗费时间");
        FIELD_MAP.put(ALL_ESTIMATE_TIME, "当前预估时间");
        FIELD_MAP.put(TAGS, "Tag");
        FIELD_MAP.put(RELATED_ISSUE, "关联" + IssueConstant.ISSUE_CN);
        FIELD_MAP.put(EPIC_SELF_NAME, "史诗名称");
        FIELD_MAP.put(PARTICIPANT, "参与人");
        FIELD_MAP.put(PRODUCT, "产品");
        FIELDS = new ArrayList<>(FIELD_MAP.keySet()).toArray(new String[FIELD_MAP.keySet().size()]);
        FIELDS_NAMES = new ArrayList<>(FIELD_MAP.values()).toArray(new String[FIELD_MAP.values().size()]);
    }

    private boolean withFeature(Long projectId, Long organizationId) {
        ResponseEntity<ProjectVO> response =
                baseFeignClient.getGroupInfoByEnableProject(organizationId, projectId);
        return response.getBody() != null;
    }

    @Override
    public void download(Long projectId,
                         Long organizationId,
                         HttpServletResponse response,
                         ExcelTemplateVO excelTemplateVO) {
        List<String> systemFields = excelTemplateVO.getSystemFields();
        List<String> customFields = excelTemplateVO.getCustomFields();
        if (ObjectUtils.isEmpty(systemFields)) {
            throw new CommonException("error.excel.header.code.empty");
        }
        boolean withFeature = (agilePluginService != null && withFeature(projectId, organizationId));

        validateSystemField(systemFields, withFeature);
        systemFields = ExcelImportTemplate.IssueHeader.addFields(systemFields);
        ExcelImportTemplate.Cursor cursor = new ExcelImportTemplate.Cursor();
        List<PredefinedDTO> predefinedList =
                processSystemFieldPredefinedList(organizationId, projectId, systemFields, withFeature, cursor);
        Map<String, String> customFieldCodeNameMap = new HashMap<>();
        String issueTypeList = ProjectCategory.getProjectIssueTypeList(projectId);
        predefinedList.addAll(processCustomFieldPredefinedList(projectId, customFields, cursor, systemFields.size(), customFieldCodeNameMap, issueTypeList));
        List<String> headers = generateExcelHeaderTitle(systemFields, customFields, customFieldCodeNameMap);
        Workbook wb = new XSSFWorkbook();
        // copy guide sheet
        copyGuideSheetFromTemplate(wb, "/templates/IssueImportGuideTemplate.xlsx");
        Sheet sheet = wb.createSheet(IMPORT_TEMPLATE_NAME);
        CellStyle style = CatalogExcelUtil.getHeadStyle(wb);
        ExcelUtil.generateHeaders(sheet, style, headers);
        try {
            //填充预定义值
            fillInPredefinedValues(wb, sheet, predefinedList);
            wb.write(response.getOutputStream());
        } catch (Exception e) {
            LOGGER.info("exception: {0}", e);
        }
    }

    protected void copyGuideSheetFromTemplate(Workbook wb, String path) {
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

    private void validateSystemField(List<String> systemFields, boolean withFeature) {
        List<String> requiredCode =
                ExcelImportTemplate
                        .IssueHeader
                        .getByRequired(true)
                        .stream()
                        .map(ExcelImportTemplate.Header::getCode)
                        .collect(Collectors.toList());
        requiredCode.forEach(r -> checkAndThrowException(systemFields, r));
        if (withFeature) {
            checkAndThrowException(systemFields, FieldCode.FEATURE);
        } else {
            checkAndThrowException(systemFields, FieldCode.EPIC_NAME);
            checkAndThrowException(systemFields, FieldCode.EPIC);
        }
    }

    private void checkAndThrowException(List<String> systemFields, String fieldCode) {
        if (!systemFields.contains(fieldCode)) {
            throw new CommonException("error.required.system.code.not.existed." + fieldCode);
        }
    }

    private List<String> generateExcelHeaderTitle(List<String> systemFields,
                                                  List<String> customFields,
                                                  Map<String, String> customFieldCodeNameMap) {
        List<String> result = new ArrayList<>();
        systemFields.forEach(s -> {
            String title = ExcelImportTemplate.IssueHeader.getValueByCode(s);
            if (StringUtils.isEmpty(title)) {
                throw new CommonException("error.excel.header.code." + s);
            }
            result.add(title);
        });
        if (!ObjectUtils.isEmpty(customFields)) {
            customFields.forEach(c -> {
                String title = customFieldCodeNameMap.get(c);
                if (StringUtils.isEmpty(title)) {
                    throw new CommonException("error.excel.header.custom.field.code." + c);
                }
                result.add(title);
            });
        }
        return result;
    }

    protected List<PredefinedDTO> processCustomFieldPredefinedList(Long projectId,
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

    private List<PredefinedDTO> processSystemFieldPredefinedList(Long organizationId,
                                                                 Long projectId,
                                                                 List<String> systemFields,
                                                                 boolean withFeature,
                                                                 ExcelImportTemplate.Cursor cursor) {
        List<PredefinedDTO> result = new ArrayList<>();
        result.add(processPriorityPredefined(organizationId, cursor, systemFields));
        result.add(processIssueTypePredefined(withFeature, projectId, cursor, systemFields));
        result.add(processParentIssuePredefined(projectId, cursor, systemFields));
        Optional
                .ofNullable(processVersionPredefined(projectId, cursor, systemFields))
                .ifPresent(result::add);
        Optional
                .ofNullable(processInfluenceVersionPredefined(projectId, cursor, systemFields))
                .ifPresent(result::add);
        Optional
                .ofNullable(processComponentPredefined(projectId, cursor, systemFields))
                .ifPresent(result::add);
        Optional
                .ofNullable(processSprintPredefined(projectId, cursor, systemFields))
                .ifPresent(result::add);
        List<String> userNameList = new ArrayList<>(getManagers(projectId).keySet());
        Optional
                .ofNullable(buildPredefinedByFieldCodeAndValues(cursor, systemFields, userNameList, FieldCode.ASSIGNEE))
                .ifPresent(result::add);
        Optional
                .ofNullable(buildPredefinedByFieldCodeAndValues(cursor, systemFields, userNameList, FieldCode.REPORTER))
                .ifPresent(result::add);
        Optional
                .ofNullable(processEpicOrFeaturePredefined(organizationId, projectId, withFeature, cursor, systemFields))
                .ifPresent(result::add);
        Optional.ofNullable(processLabelPredefined(projectId, cursor, systemFields))
                .ifPresent(result::add);

        Optional.ofNullable(buildPredefinedByFieldCodeAndValues(cursor, systemFields, userNameList, FieldCode.MAIN_RESPONSIBLE))
                .ifPresent(result::add);
        Optional.ofNullable(buildPredefinedByFieldCodeAndValues(cursor, systemFields, Arrays.asList("非生产环境", "生产环境"), FieldCode.ENVIRONMENT))
                .ifPresent(result::add);
        Optional.ofNullable(processIssueStatusPredefined(organizationId, projectId, cursor, systemFields))
                .ifPresent(result::add);
        Optional
                .ofNullable(buildPredefinedByFieldCodeAndValues(cursor, systemFields, userNameList, FieldCode.PARTICIPANT))
                .ifPresent(result::add);
        Optional.ofNullable(processIssueProductPredefined(organizationId, projectId, cursor, systemFields))
                .ifPresent(result::add);
        return result;
    }

    protected PredefinedDTO processIssueProductPredefined(Long organizationId, Long projectId, ExcelImportTemplate.Cursor cursor, List<String> fieldCodes) {
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

    protected PredefinedDTO processIssueStatusPredefined(Long organizationId, Long projectId, ExcelImportTemplate.Cursor cursor, List<String> fieldCodes) {
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

    private PredefinedDTO processIssueTypePredefined(boolean withFeature,
                                                     Long projectId,
                                                     ExcelImportTemplate.Cursor cursor,
                                                     List<String> fieldCodes) {
        List<IssueTypeVO> issueTypes = projectConfigService.queryIssueTypesByProjectId(projectId, APPLY_TYPE_AGILE, true);
        List<String> values = new ArrayList<>();
        issueTypes.forEach(i -> {
            String typeCode = i.getTypeCode();
            String typeName = i.getName();
            if (withFeature && ISSUE_EPIC.equals(typeCode)) {
                return;
            }
            if (!FEATURE.equals(typeCode)) {
                values.add(typeName);
            }
            if ("bug".equals(typeCode) && "system".equals(i.getSource())) {
                values.add(SUB_BUG_CN);
            }
        });
        int col = getColByFieldCode(fieldCodes, FieldCode.ISSUE_TYPE);
        return new PredefinedDTO(values,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.ISSUE_TYPE,
                cursor.getAndIncreaseSheetNum());
    }


    private PredefinedDTO processParentIssuePredefined(Long projectId,
                                                       ExcelImportTemplate.Cursor cursor,
                                                       List<String> systemFields) {
        //查询当前项目所有未完成的story,bug,task
        List<IssueVO> issues = issueMapper.listUndoneAvailableParents(projectId);
        int col = getColByFieldCode(systemFields, ExcelImportTemplate.IssueHeader.PARENT);
        List<String> values = new ArrayList<>();
        issues.forEach(i -> {
            String summary = i.getSummary();
            String issueNum = i.getIssueNum();
            values.add(issueNum + COLON_CN + summary);
        });
        return new PredefinedDTO(values,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                ExcelImportTemplate.IssueHeader.PARENT,
                cursor.getAndIncreaseSheetNum());
    }

    private PredefinedDTO processPriorityPredefined(Long organizationId,
                                           ExcelImportTemplate.Cursor cursor,
                                           List<String> fieldCodes) {
        List<PriorityVO> priorityVOList = priorityService.queryByOrganizationIdList(organizationId);
        List<String> priorityList =
                priorityVOList
                        .stream()
                        .filter(p -> Boolean.TRUE.equals(p.getEnable()))
                        .map(PriorityVO::getName)
                        .collect(Collectors.toList());
        int col = getColByFieldCode(fieldCodes, FieldCode.PRIORITY);
        return new PredefinedDTO(priorityList,
                PREDEFINED_VALUE_START_ROW,
                PREDEFINED_VALUE_END_ROW,
                col,
                col,
                FieldCode.PRIORITY,
                cursor.getAndIncreaseSheetNum());
    }

    private int getColByFieldCode(List<String> fieldCodes, String fieldCode) {
        int col = fieldCodes.indexOf(fieldCode);
        if (col == -1) {
            String msg = "error.fieldCodes." + fieldCode + ".not.exist";
            throw new CommonException(msg);
        }
        return col;
    }


    protected Map<String, Long> getEpicMap(Long projectId) {
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

    protected void fillInPredefinedValues(Workbook wb, Sheet sheet, List<PredefinedDTO> predefinedList) {
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

    private void isCustomFieldsIllegal(List<String> customFields, List<String> customFieldCodes) {
        customFields.forEach(c -> {
            if (!customFieldCodes.contains(c)) {
                throw new CommonException("error.illegal.custom.field.code."+ c);
            }
        });
    }

    protected Map<String, Long> getManagers(Long projectId) {
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

    protected void updateFinalRecode(FileOperationHistoryDTO fileOperationHistoryDTO, Long successCount, Long failCount, String status) {
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
        String websocketKey = WEBSOCKET_IMPORT_CODE + "-" + result.getProjectId();
        sendProcess(result, result.getUserId(), 1.0, websocketKey);
    }

    @Override
    public void sendProcess(FileOperationHistoryDTO fileOperationHistoryDTO,
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
    public FileOperationHistoryVO queryOrgLatestRecode(Long organizationId, String action) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        FileOperationHistoryDTO result = fileOperationHistoryMapper.queryOrgLatestRecode(organizationId, userId, action);
        return result == null ? new FileOperationHistoryVO() : modelMapper.map(result, FileOperationHistoryVO.class);
    }

    @Override
    public void downloadObjectSchemeField(Long organizationId, Long projectId, HttpServletResponse response) {
        Workbook wb = new XSSFWorkbook();
        //复制模板excel
        objectSchemeFieldExcelService.copyGuideSheetFromTemplate(wb);
        Sheet sheet = wb.createSheet(IMPORT_TEMPLATE_NAME);
        CellStyle style = CatalogExcelUtil.getHeadStyle(wb);
        objectSchemeFieldExcelService.generateHeaders(sheet, style);
        //填充预定义值
        objectSchemeFieldExcelService.fillInPredefinedValues(wb, sheet, projectId, organizationId);
        try {
            wb.write(response.getOutputStream());
        } catch (Exception e) {
            LOGGER.info("exception: {0}", e);
        }
    }

    @Override
    @Async
    public void batchImportObjectSchemeField(Long organizationId, Long projectId, InputStream inputStream, RequestAttributes requestAttributes) {
        Workbook workbook = ExcelUtil.getWorkbookFromInputStream(ExcelUtil.Mode.XSSF, inputStream);
        RequestContextHolder.setRequestAttributes(requestAttributes);

        Long userId = DetailsHelper.getUserDetails().getUserId();
        String socketKey = (projectId == null || projectId == 0L) ?
                WEBSOCKET_IMPORT_CUSTOM_FIELD_CODE + "org-" + organizationId : WEBSOCKET_IMPORT_CUSTOM_FIELD_CODE + "pro-" + projectId;
        FileOperationHistoryDTO history = initFileOperationHistory(
                projectId == null ? 0L : projectId,
                organizationId,
                userId,
                DOING,
                UPLOAD_FILE_CUSTOM_FIELD,
                socketKey);

        if (!objectSchemeFieldExcelService.validExcelTemplate(workbook, history)) {
            FileOperationHistoryDTO errorImport = fileOperationHistoryMapper.selectByPrimaryKey(history.getId());
            sendProcess(errorImport, history.getUserId(), 0.0, socketKey);
            LOGGER.error("导入文件为空，导入失败");
            return;
        }

        Sheet sheet = workbook.getSheetAt(1);
        int realRowCount = objectSchemeFieldExcelService.getRealRowCount(sheet);
        int lastSendCountNum = 0;
        ExcelImportTemplate.Progress progress = new ExcelImportTemplate.Progress();

        List<Long> importedFieldIds = new ArrayList<>();
        Map<Integer, List<Integer>> errorRowColMap = new HashMap<>(sheet.getPhysicalNumberOfRows());
        List<IssueTypeVO> issueTypes = objectSchemeFieldService.issueTypes(organizationId, projectId);
        Map<String, IssueTypeVO> issueTypeNameMap = issueTypes.stream().filter(IssueTypeVO::getEnabled).collect(Collectors.toMap(IssueTypeVO::getName, a -> a, (k1, k2) -> k1));
        Map<String, UserVO> userNameMap = objectSchemeFieldExcelService.getUserNameMap(organizationId, projectId);
        for (int r = 1; r < sheet.getPhysicalNumberOfRows(); r++) {
            Row row = sheet.getRow(r);
            if (objectSchemeFieldExcelService.isSkip(row)
                    || (r > 1 && objectSchemeFieldExcelService.isKeyValue(sheet.getRow(r - 1))
                    && objectSchemeFieldExcelService.isExtendKeyValue(row))) {
                continue;
            }
            if (objectSchemeFieldExcelService.checkCanceled(
                    organizationId,
                    projectId,
                    history.getId(),
                    importedFieldIds)) {
                return;
            }
            ObjectSchemeFieldCreateVO objectSchemeFieldCreate =
                    objectSchemeFieldExcelService.generateObjectSchemeField(organizationId, projectId, row, errorRowColMap, issueTypeNameMap);
            int keyRowNum = r + 1;
            while (objectSchemeFieldExcelService.isExtendKeyValue(sheet.getRow(keyRowNum))) {
                objectSchemeFieldExcelService.setKeyValue(objectSchemeFieldCreate, sheet.getRow(keyRowNum));
                keyRowNum++;
            }
            Map<String, Integer> keyRowMap = objectSchemeFieldExcelService.validKeyValue(objectSchemeFieldCreate, sheet, r, errorRowColMap);
            objectSchemeFieldExcelService.validAndSetDefaultValue(objectSchemeFieldCreate, keyRowMap, row, userNameMap, errorRowColMap);
            if (ObjectUtils.isEmpty(errorRowColMap.get(r))) {
                objectSchemeFieldExcelService.createObjectSchemeField(projectId, organizationId, objectSchemeFieldCreate, issueTypes);
                progress.successCountIncrease();
                history.setSuccessCount(progress.getSuccessCount());
            } else {
                progress.failCountIncrease();
                history.setFailCount(progress.getFailCount());
            }
            progress.processNumIncrease();
            if ((progress.getProcessNum() - lastSendCountNum) * 1.0 / realRowCount >= 0.1) {
                lastSendCountNum = progress.getProcessNum();
                sendProcess(history, userId, (progress.getProcessNum() * 1.0 / realRowCount) * 0.97 * 100, socketKey);
            }
        }
        //错误数据生成excel
        String status;
        if (ObjectUtils.isEmpty(errorRowColMap)) {
            status = SUCCESS;
        } else {
            objectSchemeFieldExcelService.
                    generateErrorDataExcelAndUpload(errorRowColMap, workbook, sheet, history, organizationId);
            status = FAILED;
        }

        //发送完成进度消息
        FileOperationHistoryDTO update = new FileOperationHistoryDTO();
        update.setId(history.getId());
        update.setSuccessCount(progress.getSuccessCount());
        update.setFailCount(progress.getFailCount());
        update.setStatus(status);
        update.setFileUrl(history.getFileUrl());
        update.setObjectVersionNumber(history.getObjectVersionNumber());
        fileOperationHistoryMapper.updateByPrimaryKeySelective(update);
        FileOperationHistoryDTO result = fileOperationHistoryMapper.selectByPrimaryKey(update.getId());
        sendProcess(result, result.getUserId(), 100.0, socketKey);
    }


    protected String uploadErrorExcel(Workbook errorWorkbook, Long organizationId) {
        // 上传错误的excel
        MultipartFile multipartFile = new MultipartExcelUtil(MULTIPART_NAME, ORIGINAL_FILE_NAME, errorWorkbook);
        return fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), null, FILE_NAME, multipartFile);
    }

    protected Boolean checkEpicNameExist(Long projectId, String epicName) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setEpicName(epicName);
        List<IssueDTO> issueDTOList = issueMapper.select(issueDTO);
        return issueDTOList == null || issueDTOList.isEmpty();
    }

    protected Boolean checkCanceled(Long projectId, Long fileOperationHistoryId, List<Long> importedIssueIds) {
        FileOperationHistoryDTO checkCanceledDO = fileOperationHistoryMapper.selectByPrimaryKey(fileOperationHistoryId);
        if (UPLOAD_FILE.equals(checkCanceledDO.getAction())
                && CANCELED.equals(checkCanceledDO.getStatus())) {
            if (!importedIssueIds.isEmpty()) {
                LOGGER.info(importedIssueIds.toString());
                issueService.batchDeleteIssuesAgile(projectId, importedIssueIds);
            }
            return true;
        }
        return false;
    }

    /**
     * @param sheet
     * @param columnNum 数据页总共有多少列数据
     * @return
     */
    protected Integer getRealRowCount(Sheet sheet, int columnNum) {
        Integer count = 0;
        for (int r = 1; r <= sheet.getPhysicalNumberOfRows(); r++) {
            Row row = sheet.getRow(r);
            //row为空跳过
            if (isSkip(row, columnNum)) {
                continue;
            }
            count++;
        }
        return count;
    }

    protected boolean isSkip(Row row, int columnNum) {
        if (row == null) {
            return true;
        }
        //所有列都为空才跳过
        boolean skip = true;
        for (int i = 0; i < columnNum; i++) {
            Cell cell = row.getCell(i);
            skip = skip && isCellEmpty(cell);

        }
        return skip;
    }

    protected boolean isCellEmpty(Cell cell) {
        return cell == null || cell.toString().equals("") || cell.getCellTypeEnum() == CellType.BLANK;
    }

    @Async
    @Override
    public void batchImport(Long projectId,
                            Long organizationId,
                            Long userId,
                            InputStream inputStream,
                            ServletRequestAttributes requestAttributes) {
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Workbook workbook = ExcelUtil.getWorkbookFromInputStream(ExcelUtil.Mode.XSSF, inputStream);
        String websocketKey = WEBSOCKET_IMPORT_CODE + "-" + projectId;
        FileOperationHistoryDTO history = initFileOperationHistory(projectId, userId, DOING, UPLOAD_FILE, websocketKey);
        validateWorkbook(workbook, history, websocketKey);
        List<String> headerNames = resolveCodeFromHeader(workbook, history, websocketKey);
        Map<Integer, ExcelColumnVO> headerMap = new LinkedHashMap<>();
        boolean withFeature = (agilePluginService != null && withFeature(projectId, organizationId));
        //获取日期类型的列
        Set<Integer> dateTypeColumns = new HashSet<>();
        processHeaderMap(projectId, organizationId, headerNames, headerMap, withFeature, history, dateTypeColumns, websocketKey);
        validateRequiredSystemField(headerMap, withFeature, history);

        Sheet dataSheet = workbook.getSheetAt(1);
        int columnNum = headerMap.size();
        Integer dataRowCount = getRealRowCount(dataSheet, columnNum);
        Map<Integer, Set<Integer>> parentSonMap = new HashMap<>();
        Map<Integer, Integer> sonParentMap = new HashMap<>();
        Set<Integer> withoutParentRows = new HashSet<>();
        int issueTypeCol = getColIndexByFieldCode(headerMap, FieldCode.ISSUE_TYPE);
        int parentCol = getColIndexByFieldCode(headerMap, ExcelImportTemplate.IssueHeader.PARENT);
        processParentSonRelationship(parentSonMap, sonParentMap, withoutParentRows, dataSheet, dataRowCount, columnNum, issueTypeCol, parentCol, headerMap);
        ExcelImportTemplate.Progress progress = new ExcelImportTemplate.Progress();
        //key为错误的行数，value为错误的列
        Map<Integer, List<Integer>> errorRowColMap = new HashMap<>();
        List<Long> importedIssueIds = new ArrayList<>();
        Map<Integer, Long> rowIssueIdMap = new HashMap<>();
        List<RelatedIssueVO> relatedIssueList = new ArrayList<>();
        int lastSendCountNum = 0;
        Map<Long,List<String>> requireFieldMap = new HashMap<>();
        List<TriggerCarrierVO> triggerCarrierVOS = new ArrayList<>();
        for (int rowNum = 1; rowNum <= dataRowCount; rowNum++) {
            if (Boolean.TRUE.equals(checkCanceled(projectId, history.getId(), importedIssueIds))) {
                return;
            }
            Row row = dataSheet.getRow(rowNum);
            if (isSkip(row, columnNum)) {
                continue;
            }
            for (int col = 0; col < columnNum; col++) {
                Cell cell = row.getCell(col);
                if (cell != null && !dateTypeColumns.contains(col)) {
                    row.getCell(col).setCellType(CellType.STRING);
                }
            }
            Cell issueTypeCell = row.getCell(issueTypeCol);
            if (issueTypeCell == null) {
                issueTypeCell = row.createCell(issueTypeCol);
            }
            String issueType;
            if (isCellEmpty(issueTypeCell)) {
                errorRowColMap.put(rowNum, Arrays.asList(issueTypeCol));
                issueTypeCell.setCellValue(buildWithErrorMsg("", IssueConstant.ISSUE_TYPE_CN + "为空"));
                progress.failCountIncrease();
                progress.processNumIncrease();
                history.setFailCount(progress.getFailCount());
                if ((progress.getProcessNum() - lastSendCountNum) * 1.0 / dataRowCount >= 0.1) {
                    lastSendCountNum = progress.getProcessNum();
                    sendProcess(history, userId, progress.getProcessNum() * 1.0 / dataRowCount, websocketKey);
                }
                continue;
            } else {
                issueType = issueTypeCell.toString();
            }
            String issueTypeCode = getIssueTypeCode(headerMap, issueType);
            Set<Integer> sonSet = parentSonMap.get(rowNum);
            boolean hasSonNodes = !ObjectUtils.isEmpty(sonSet);

            if ((IssueTypeCode.isStory(issueTypeCode)
                    || IssueTypeCode.isTask(issueTypeCode)
                    || IssueTypeCode.isBug(issueTypeCode))
                    && hasSonNodes) {
                List<Long> insertIds = new ArrayList<>();
                try {
                    IssueCreateVO parent = new IssueCreateVO();
                    validateData(projectId, row, headerMap, withoutParentRows, errorRowColMap, parent, null, issueTypeCol, parentCol, requireFieldMap);
                    if (!ObjectUtils.isEmpty(errorRowColMap.get(rowNum))) {
                        lastSendCountNum = processErrorData(userId, history, dataSheet, dataRowCount, progress, errorRowColMap, rowNum, sonSet, parentCol, lastSendCountNum);
                        rowNum = Collections.max(sonSet);
                        continue;
                    }
                    List<ComponentIssueRelVO> components =  parent.getComponentIssueRelVOList();
                    Long sprintId = parent.getSprintId();
                    Long epicId = parent.getEpicId();
                    Optional.ofNullable(parent.getRelatedIssueVO()).ifPresent(relatedIssueList::add);
                    IssueVO result = stateMachineClientService.createIssueWithoutRuleNotice(parent, APPLY_TYPE_AGILE);
                    insertIds.add(result.getIssueId());
                    insertCustomFields(result.getIssueId(), parent.getCustomFields(), projectId);
                    List<Long> customFieldIds = new ArrayList<>();
                    if (!CollectionUtils.isEmpty(parent.getCustomFields())) {
                        customFieldIds.addAll(parent.getCustomFields().stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList()));
                    }
                    issueService.buildTriggerCarrierVO(projectId, result.getIssueId(), triggerCarrierVOS, customFieldIds);
                    rowIssueIdMap.put(rowNum, result.getIssueId());

                    result.setComponentIssueRelVOList(components);
                    result.setSprintId(sprintId);
                    result.setEpicId(epicId);

                    boolean sonsOk = true;
                    Map<Integer, IssueCreateVO> sonMap = new HashMap<>();
                    for (Integer sonRowNum : sonSet) {
                        IssueCreateVO son = new IssueCreateVO();
                        Row sonRow = dataSheet.getRow(sonRowNum);
                        for (int col = 0; col < columnNum; col++) {
                            Cell cell = sonRow.getCell(col);
                            if (cell != null && !dateTypeColumns.contains(col)) {
                                sonRow.getCell(col).setCellType(CellType.STRING);
                            }
                        }
                        validateData(projectId, sonRow, headerMap, withoutParentRows, errorRowColMap, son, result, issueTypeCol, parentCol, requireFieldMap);
                        if (!ObjectUtils.isEmpty(errorRowColMap.get(sonRowNum))) {
                            sonsOk = false;
                            break;
                        } else {
                            sonMap.put(sonRowNum, son);
                        }
                    }
                    if (!sonsOk) {
                        lastSendCountNum = processErrorData(userId, history, dataSheet, dataRowCount, progress, errorRowColMap, rowNum, sonSet, parentCol, lastSendCountNum);
                        rowNum = Collections.max(sonSet);
                        issueService.batchDeleteIssuesAgile(projectId, insertIds);
                        continue;
                    }
                    sonMap.forEach((k, v) -> {
                        Optional.ofNullable(v.getRelatedIssueVO()).ifPresent(relatedIssueList::add);
                        IssueVO returnValue = stateMachineClientService.createIssueWithoutRuleNotice(v, APPLY_TYPE_AGILE);
                        insertIds.add(returnValue.getIssueId());
                        insertCustomFields(returnValue.getIssueId(), v.getCustomFields(), projectId);
                        List<Long> subIssueCustomFieldIds = new ArrayList<>();
                        if (!CollectionUtils.isEmpty(v.getCustomFields())) {
                            subIssueCustomFieldIds.addAll(v.getCustomFields().stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList()));
                        }
                        issueService.buildTriggerCarrierVO(projectId, returnValue.getIssueId(), triggerCarrierVOS, subIssueCustomFieldIds);
                        rowIssueIdMap.put(k, returnValue.getIssueId());
                    });

                    importedIssueIds.add(result.getIssueId());
                    importedIssueIds.addAll(rowIssueIdMap.values());
                    progress.addSuccessCount(sonSet.size() + 1L);
                    progress.addProcessNum(sonSet.size() + 1);
                    rowNum = Collections.max(sonSet);
                } catch (Exception e) {
                    LOGGER.error("insert data error when import excel, exception: {0}", e);
                    lastSendCountNum = processErrorData(userId, history, dataSheet, dataRowCount, progress, errorRowColMap, rowNum, sonSet, parentCol, lastSendCountNum);
                    rowNum = Collections.max(sonSet);
                    issueService.batchDeleteIssuesAgile(projectId, insertIds);
                    continue;
                }
            } else {
                IssueCreateVO issueCreateVO = new IssueCreateVO();
                validateData(projectId, row, headerMap, withoutParentRows, errorRowColMap, issueCreateVO, null, issueTypeCol, parentCol, requireFieldMap);
                if (!ObjectUtils.isEmpty(errorRowColMap.get(rowNum))) {
                    progress.failCountIncrease();
                    progress.processNumIncrease();
                    history.setFailCount(progress.getFailCount());
                    if ((progress.getProcessNum() - lastSendCountNum) * 1.0 / dataRowCount >= 0.1) {
                        lastSendCountNum = progress.getProcessNum();
                        sendProcess(history, userId, progress.getProcessNum() * 1.0 / dataRowCount, websocketKey);
                    }
                    continue;
                }
                Optional.ofNullable(issueCreateVO.getRelatedIssueVO()).ifPresent(relatedIssueList::add);
                IssueVO result = stateMachineClientService.createIssueWithoutRuleNotice(issueCreateVO, APPLY_TYPE_AGILE);
                insertCustomFields(result.getIssueId(), issueCreateVO.getCustomFields(), projectId);
                List<Long> customFieldIds = new ArrayList<>();
                if (!CollectionUtils.isEmpty(issueCreateVO.getCustomFields())) {
                    customFieldIds.addAll(issueCreateVO.getCustomFields().stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList()));
                }
                issueService.buildTriggerCarrierVO(projectId, result.getIssueId(), triggerCarrierVOS, customFieldIds);
                rowIssueIdMap.put(rowNum, result.getIssueId());

                importedIssueIds.add(result.getIssueId());
                progress.successCountIncrease();
                progress.processNumIncrease();
            }
            history.setFailCount(progress.getFailCount());
            history.setSuccessCount(progress.getSuccessCount());
            if ((progress.getProcessNum() - lastSendCountNum) * 1.0 / dataRowCount >= 0.1) {
                lastSendCountNum = progress.getProcessNum();
                sendProcess(history, userId, progress.getProcessNum() * 1.0 / dataRowCount, websocketKey);
            }
        }
        updateRelatedIssue(relatedIssueList, rowIssueIdMap, errorRowColMap, headerMap, dataSheet, projectId, progress, parentSonMap, parentCol);
        issueService.batchCreateIssueInvokeTrigger(triggerCarrierVOS);
        //错误数据生成excel
        String status;
        if (ObjectUtils.isEmpty(errorRowColMap)) {
            status = SUCCESS;
        } else {
            generateErrorDataExcelAndUpload(errorRowColMap, dataSheet, headerMap, headerNames, history, organizationId);
            status = FAILED;
        }
        updateFinalRecode(history, progress.getSuccessCount(), progress.getFailCount(), status);
    }

    private void validateRequiredSystemField(Map<Integer, ExcelColumnVO> headerMap,
                                             boolean withFeature,
                                             FileOperationHistoryDTO history) {
        List<String> headerCodes = new ArrayList<>();
        headerMap.forEach((k, v) -> {
            if (!v.isCustomField()) {
                headerCodes.add(v.getFieldCode());
            }
        });
        try {
            validateSystemField(headerCodes, withFeature);
        } catch (CommonException e) {
            history.setStatus("template_error_missing_required_column");
            fileOperationHistoryMapper.updateByPrimaryKeySelective(history);
            FileOperationHistoryDTO errorImport = fileOperationHistoryMapper.selectByPrimaryKey(history.getId());
            String websocketKey = WEBSOCKET_IMPORT_CODE + "-" + errorImport.getProjectId();
            sendProcess(errorImport, history.getUserId(), 0.0, websocketKey);
            throw e;
        }
    }

    private void updateRelatedIssue(List<RelatedIssueVO> relatedIssueList,
                                    Map<Integer, Long> rowIssueIdMap,
                                    Map<Integer, List<Integer>> errorRowColMap,
                                    Map<Integer, ExcelColumnVO> headerMap,
                                    Sheet dataSheet,
                                    Long projectId,
                                    ExcelImportTemplate.Progress progress,
                                    Map<Integer, Set<Integer>> parentSonMap,
                                    int parentCol) {
        relatedIssueList =
                relatedIssueList
                        .stream()
                        .filter(x -> !ObjectUtils.isEmpty(x.getRelatedIds()) || !ObjectUtils.isEmpty(x.getRelatedRows())).collect(Collectors.toList());
        Set<Long> deleteIssueIds = new HashSet<>();
        Map<Long, Set<Long>> relatedMap = new HashMap<>();
        int relateIssueIndex = 0;
        for (Map.Entry<Integer, ExcelColumnVO> entry : headerMap.entrySet()) {
            if (entry.getValue().getFieldCode().equals(ExcelImportTemplate.IssueHeader.RELATE_ISSUE)) {
                relateIssueIndex = entry.getKey();
            }
        }
        for (RelatedIssueVO x : relatedIssueList) {
            Integer rowNum = x.getRow();
            Cell cell = dataSheet.getRow(rowNum).getCell(relateIssueIndex);
            String value = cell.toString();
            Long issueId = rowIssueIdMap.get(rowNum);
            if (issueId != null && ObjectUtils.isEmpty(errorRowColMap.get(rowNum))) {
                Set<Integer> relatedRows = x.getRelatedRows();
                Set<Long> relatedIssueIds = x.getRelatedIds();
                boolean ok = true;
                for (Integer relatedRow : relatedRows) {
                    if (Objects.equals(rowNum, relatedRow)) {
                        deleteIssueIds.add(issueId);
                        cell.setCellValue(buildWithErrorMsg(value, "自己不能和自己关联，rowNum: " + (rowNum + 1)));
                        addErrorColumn(rowNum, relateIssueIndex, errorRowColMap);
                        ok = false;
                        progress.failCountIncrease();
                        progress.successCountDecrease();
                        Set<Integer> sonSet = parentSonMap.get(rowNum);
                        if (!CollectionUtils.isEmpty(sonSet)) {
                            sonSet.forEach(v -> {
                                Long sonIssueId = rowIssueIdMap.get(v);
                                if (!ObjectUtils.isEmpty(sonIssueId)) {
                                    deleteIssueIds.add(sonIssueId);
                                    progress.failCountIncrease();
                                    progress.successCountDecrease();
                                }
                            });
                            setErrorMsgToParentSonRow(rowNum, dataSheet, errorRowColMap, sonSet, parentCol);
                        }
                        break;
                    }
                    Long relatedIssueId = rowIssueIdMap.get(relatedRow);
                    if (relatedIssueId == null) {
                        deleteIssueIds.add(issueId);
                        cell.setCellValue(buildWithErrorMsg(value, "第" + (relatedRow + 1) + "行" + IssueConstant.ISSUE_CN + "不存在"));
                        addErrorColumn(rowNum, relateIssueIndex, errorRowColMap);
                        ok = false;
                        progress.failCountIncrease();
                        progress.successCountDecrease();
                        Set<Integer> sonSet = parentSonMap.get(rowNum);
                        if (!CollectionUtils.isEmpty(sonSet)) {
                            sonSet.forEach(v -> {
                                Long sonIssueId = rowIssueIdMap.get(v);
                                if (!ObjectUtils.isEmpty(sonIssueId)) {
                                    deleteIssueIds.add(sonIssueId);
                                    progress.failCountIncrease();
                                    progress.successCountDecrease();
                                }
                            });
                            setErrorMsgToParentSonRow(rowNum, dataSheet, errorRowColMap, sonSet, parentCol);
                        }
                        break;
                    } else {
                        relatedIssueIds.add(relatedIssueId);
                    }
                }
                if (ok) {
                    relatedMap.put(issueId, relatedIssueIds);
                }
            }
        }
        if (!deleteIssueIds.isEmpty()) {
            issueService.batchDeleteIssuesAgile(projectId, new ArrayList<>(deleteIssueIds));
        }

        List<IssueLinkTypeDTO> issueLinkTypeDTOS = issueLinkTypeMapper.queryIssueLinkTypeByProjectId(projectId, null, "关联", null);
        if (!CollectionUtils.isEmpty(issueLinkTypeDTOS)) {
            Long linkTypeId =
                    issueLinkTypeMapper.queryIssueLinkTypeByProjectId(projectId, null, "关联", null)
                            .get(0)
                            .getLinkTypeId();
            relatedMap.forEach((k, v) -> {
                Long issueId = k;
                Set<Long> linkedIssueIds = v;
                List<IssueLinkCreateVO> issueLinkList = new ArrayList<>();
                linkedIssueIds.forEach(l -> {
                    IssueLinkCreateVO create = new IssueLinkCreateVO();
                    create.setIssueId(issueId);
                    create.setLinkTypeId(linkTypeId);
                    create.setLinkedIssueId(l);
                    issueLinkList.add(create);
                });
                issueLinkService.createIssueLinkList(issueLinkList, issueId, projectId);
            });
        }
    }

    protected void insertCustomFields(Long issueId,
                                    List<PageFieldViewUpdateVO> customFields,
                                    Long projectId) {
        if (!ObjectUtils.isEmpty(customFields)) {
            BatchUpdateFieldsValueVo batchUpdateFieldsValueVo = new BatchUpdateFieldsValueVo();
            batchUpdateFieldsValueVo.setCustomFields(customFields);
            batchUpdateFieldsValueVo.setIssueIds(Arrays.asList(issueId));
            batchUpdateFieldsValueVo.setPredefinedFields(new JSONObject());
            fieldValueService.handlerCustomFields(projectId, customFields, "agile_issue", batchUpdateFieldsValueVo.getIssueIds(), null, false, new HashMap<>());
            //导入创建问题通知自定义字段人员
            IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
            IssueVO result = issueAssembler.issueDetailDTOToVO(issue, new HashMap<>(), new HashMap<>(), new HashMap<>());
            sendMsgUtil.sendMsgToCustomFieldUsersByIssueCreate(projectId, result, DetailsHelper.getUserDetails().getUserId());
        }
    }

    private void generateErrorDataExcelAndUpload(Map<Integer, List<Integer>> errorRowColMap,
                                                 Sheet dataSheet,
                                                 Map<Integer, ExcelColumnVO> headerMap,
                                                 List<String> headerNames,
                                                 FileOperationHistoryDTO history,
                                                 Long organizationId) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        copyGuideSheetFromTemplate(workbook, "/templates/IssueImportGuideTemplate.xlsx");
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

    protected void writeErrorData(Map<Integer, List<Integer>> errorRowColMap,
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

    protected List<PredefinedDTO> processPredefinedByHeaderMap(Map<Integer, ExcelColumnVO> headerMap) {
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

    private int processErrorData(Long userId,
                                  FileOperationHistoryDTO history,
                                  Sheet dataSheet,
                                  Integer dataRowCount,
                                  ExcelImportTemplate.Progress progress,
                                  Map<Integer, List<Integer>> errorRowColMap,
                                  int rowNum,
                                  Set<Integer> sonSet,
                                  int parentColIndex, int lastSendCountNum) {
        setErrorMsgToParentSonRow(rowNum, dataSheet, errorRowColMap, sonSet, parentColIndex);
        int errorCount = sonSet.size() + 1;
        Long failCount = progress.getFailCount() + errorCount;
        history.setFailCount(failCount);
        int processNum = progress.getProcessNum() + errorCount;
        progress.setFailCount(failCount);
        progress.addProcessNum(errorCount);
        String websocketKey = WEBSOCKET_IMPORT_CODE + "-" + history.getProjectId();
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

    private void validateData(Long projectId,
                              Row row,
                              Map<Integer, ExcelColumnVO> headerMap,
                              Set<Integer> withoutParentRows,
                              Map<Integer, List<Integer>> errorRowColMap,
                              IssueCreateVO issueCreateVO,
                              IssueVO parentIssue,
                              int issueTypeCol,
                              int parentCol,
                              Map<Long, List<String>> requireFieldMap) {
        issueCreateVO.setProjectId(projectId);
        int rowNum = row.getRowNum();
        Cell issueTypeCell = row.getCell(issueTypeCol);
        if (issueTypeCell == null) {
            issueTypeCell = row.createCell(issueTypeCol);
        }
        String value = "";
        if (isCellEmpty(issueTypeCell)) {
            issueTypeCell.setCellValue(buildWithErrorMsg(value, IssueConstant.ISSUE_TYPE_CN + "为空"));
            addErrorColumn(rowNum, issueTypeCol, errorRowColMap);
            return;
        }
        value = issueTypeCell.toString();
        if (withoutParentRows.contains(rowNum)) {
            issueTypeCell.setCellValue(buildWithErrorMsg(value, "子任务/子缺陷必须要有父节点"));
            addErrorColumn(rowNum, issueTypeCol, errorRowColMap);
            return;
        }
        String issueTypeCode = getIssueTypeCode(headerMap, value);
        if (parentIssue == null
                && (IssueTypeCode.isSubTask(issueTypeCode)
                || SUB_BUG_CN.equals(value))) {
            Cell parentCell = row.getCell(parentCol);
            if (parentCell == null) {
                parentCell = row.createCell(parentCol);
            }
            String parentCellValue = "";
            if (isCellEmpty(parentCell)) {
                issueTypeCell.setCellValue(buildWithErrorMsg(parentCellValue, "子任务/子缺陷必须要有父节点"));
                addErrorColumn(rowNum, parentCol, errorRowColMap);
                return;
            }
            parentCellValue = parentCell.toString();
            List<String> values = headerMap.get(parentCol).getPredefinedValues();
            String issueNum = parentCellValue.split(COLON_CN)[0];
            if (!values.contains(issueNum)) {
                parentCell.setCellValue(buildWithErrorMsg(parentCellValue, "输入的父级编号有误"));
                addErrorColumn(rowNum, parentCol, errorRowColMap);
                return;
            }
            parentIssue = issueMapper.selectByIssueNum(projectId, issueNum);
            if (parentIssue == null) {
                issueTypeCell.setCellValue(buildWithErrorMsg(parentCellValue, "父节点不存在"));
                addErrorColumn(rowNum, parentCol, errorRowColMap);
                return;
            }
            IssueDTO issueDTO = issueMapper.queryIssueSprintNotClosed(projectId, parentIssue.getIssueId());
            parentIssue.setSprintId(issueDTO.getSprintId());
        }
        for (Map.Entry<Integer, ExcelColumnVO> entry : headerMap.entrySet()) {
            Integer col = entry.getKey();
            ExcelColumnVO excelColumn = entry.getValue();
            boolean isCustomField = excelColumn.isCustomField();
            Boolean checkRequireField = checkRequireField(requireFieldMap, excelColumn, issueCreateVO, row, col, errorRowColMap);
            if (!checkRequireField) {
                break;
            }
            if (isCustomField) {
                validateCustomFieldData(row, col, excelColumn, errorRowColMap, issueCreateVO);
            } else {
                validateSystemFieldData(row, col, excelColumn, errorRowColMap, issueCreateVO, parentIssue, projectId, headerMap);
            }
            handlerRequireFiled(excelColumn, requireFieldMap, issueCreateVO, projectId);
        }
    }

    protected Boolean checkRequireField(Map<Long, List<String>> requireFieldMap, ExcelColumnVO excelColum, IssueCreateVO issueCreateVO, Row row, Integer col, Map<Integer, List<Integer>> errorRowColMap) {
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

    protected void handlerRequireFiled(ExcelColumnVO excelColumn, Map<Long, List<String>> requireFieldMap, IssueCreateVO issueCreateVO, Long projectId){
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

    protected void validateCustomFieldData(Row row,
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

    protected List<String> splitByRegex(String value) {
        String regex1 = ",";
        String regex2 = "，";
        List<String> result = new ArrayList<>();
        String[] array = value.split(regex1);
        for (String str : array) {
            result.addAll(Arrays.asList(str.split(regex2)));
        }
        return result;
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

    protected Integer getColIndexByFieldCode(Map<Integer, ExcelColumnVO> headerMap, String fieldCode) {
        for (Map.Entry<Integer, ExcelColumnVO> entry : headerMap.entrySet()) {
            if (entry.getValue().getFieldCode().equals(fieldCode)) {
                return entry.getKey();
            }
        }
        return null;
    }

    private void validateSystemFieldData(Row row,
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
                validateAndSetFeature(row, col, excelColumn, errorRowColMap, issueCreateVO, issueTypeCode);
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
                validateAndSetMainResponsible(row, col, issueCreateVO, errorRowColMap, excelColumn);
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

    protected void validateAndSetIssueStatus(Row row,
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

    private void validateAndSetMainResponsible(Row row,
                                               Integer col,
                                               IssueCreateVO issueCreateVO,
                                               Map<Integer, List<Integer>> errorRowColMap,
                                               ExcelColumnVO excelColumnVO) {
        Cell cell = row.getCell(col);
        int rowNum = row.getRowNum();
        if (!isCellEmpty(cell)) {
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
                            Boolean isSubTask = "sub_task".equals(issueVO.getTypeCode());
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

    protected void validateAndSetEstimatedTime(Row row,
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

    protected void validateAndSetActualTime(Row row,
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
        } else {
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

    protected void setDescription(Row row,
                                Integer col,
                                IssueCreateVO issueCreateVO) {
        Cell cell = row.getCell(col);
        if (!isCellEmpty(cell)) {
            String value = cell.toString();
            issueCreateVO.setDescription("<p>"+ value + "</p>");
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


    protected void validateAndSetSummary(Row row,
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

    private void validateAndSetEpic(Row row,
                                    Integer col,
                                    ExcelColumnVO excelColumn,
                                    Map<Integer, List<Integer>> errorRowColMap,
                                    IssueCreateVO issueCreateVO,
                                    String issueTypeCode,
                                    IssueVO parentIssue,
                                    String issueType) {
        if(!IssueTypeCode.isSubTask(issueTypeCode)
                && !IssueTypeCode.isEpic(issueTypeCode)) {
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

    private void validateAndSetFeature(Row row,
                                       Integer col,
                                       ExcelColumnVO excelColumn,
                                       Map<Integer, List<Integer>> errorRowColMap,
                                       IssueCreateVO issueCreateVO,
                                       String issueTypeCode) {
        if (IssueTypeCode.isStory(issueTypeCode)) {
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

    protected void validateAndSetEpicName(Row row,
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

    protected void validateAndSetIssueType(Row row,
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
                versionIssueRelVO.setRelationType(RELATION_TYPE_FIX);
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

    protected void validateAndSetReporter(Row row,
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

    protected void addErrorColumn(int rowNum, Integer col, Map<Integer, List<Integer>> errorRowColMap) {
        List<Integer> columns = errorRowColMap.computeIfAbsent(rowNum, k -> new ArrayList<>());
        columns.add(col);
    }

    protected String buildWithErrorMsg(String value, String msg) {
        return new StringBuilder(value).append("(").append(msg).append(")").toString();
    }

    private void processParentSonRelationship(Map<Integer, Set<Integer>> parentSonMap,
                                              Map<Integer, Integer> sonParentMap,
                                              Set<Integer> withoutParentRows,
                                              Sheet dataSheet,
                                              Integer dataRowCount,
                                              Integer columnNum,
                                              int issueTypeCol,
                                              int parentCol,
                                              Map<Integer, ExcelColumnVO> headerMap) {
        Map<Integer, String> rowIssueTypeMap = new LinkedHashMap<>();
        List<IssueTypeLinkDTO> issueTypeLinks = new ArrayList<>();
        for (int i = 1; i <= dataRowCount; i++) {
            int size = issueTypeLinks.size();
            IssueTypeLinkDTO lastIssueTypeLink = null;
            if (size > 0) {
                lastIssueTypeLink = issueTypeLinks.get(size - 1);
            }
            Row row = dataSheet.getRow(i);
            if (isSkip(row, columnNum)) {
                continue;
            }
            String issueType = getCellString(row.getCell(issueTypeCol));
            if (issueType == null) {
                continue;
            }
            IssueTypeLinkDTO issueTypeLink = new IssueTypeLinkDTO(i, issueType);
            issueTypeLinks.add(issueTypeLink);
            if (lastIssueTypeLink != null) {
                lastIssueTypeLink.setNext(issueTypeLink);
            }
            rowIssueTypeMap.put(i, issueType);
        }
        parentSonMap.putAll(getParentSonMap(issueTypeLinks, headerMap));
        sonParentMap.putAll(getSonParentMap(parentSonMap));

        for (Map.Entry<Integer, String> entry : rowIssueTypeMap.entrySet()) {
            Integer rowNum = entry.getKey();
            String issueType = entry.getValue();
            String issueTypeCode = getIssueTypeCode(headerMap, issueType);
            if (IssueTypeCode.isSubTask(issueTypeCode)
                    || SUB_BUG_CN.equals(issueType)) {
                Integer parentRow = sonParentMap.get(rowNum);
                if (parentRow == null) {
                    Cell parentCell = dataSheet.getRow(rowNum).getCell(parentCol);
                    if (isCellEmpty(parentCell)) {
                        withoutParentRows.add(rowNum);
                    }
                }
            }
        }
    }

    private String getCellString(Cell cell) {
        if (isCellEmpty(cell)) {
            return null;
        }
        return cell.toString();
    }

    private void processHeaderMap(Long projectId,
                                  Long organizationId,
                                  List<String> headerNames,
                                  Map<Integer, ExcelColumnVO> headerMap,
                                  boolean withFeature,
                                  FileOperationHistoryDTO history,
                                  Set<Integer> dateTypeColumns,
                                  String websocketKey) {
        boolean containsCustomFields = false;
        for (int i = 0; i < headerNames.size(); i++) {
            String headerName = headerNames.get(i);
            String code = ExcelImportTemplate.IssueHeader.getCodeByValue(headerName);
            boolean isSystemField = !StringUtils.isEmpty(code);
            ExcelColumnVO excelColumnVO = new ExcelColumnVO();
            headerMap.put(i, excelColumnVO);
            excelColumnVO.setCustomField(!isSystemField);
            if (isSystemField) {
                addSystemFieldIfDateType(dateTypeColumns, code, i, excelColumnVO);
                excelColumnVO.setFieldCode(code);
                setSystemFieldPredefinedValueByCode(code, projectId, organizationId, excelColumnVO, withFeature);
            } else {
                containsCustomFields = true;
                excelColumnVO.setFieldCode(headerName);
            }
        }
        if (containsCustomFields) {
            String issueTypeList = ProjectCategory.getProjectIssueTypeList(projectId);
            validateCustomField(headerMap, projectId, history, issueTypeList, dateTypeColumns, websocketKey);
        }
    }

    protected void addSystemFieldIfDateType(Set<Integer> dateTypeColumns,
                                          String code,
                                          int col,
                                          ExcelColumnVO excelColumnVO) {
        if (Arrays.asList(SYSTEM_DATE_FIELD_LIST).contains(code)) {
            dateTypeColumns.add(col);
            excelColumnVO.setDateType(true);
        }
    }

    protected void validateCustomField(Map<Integer, ExcelColumnVO> headerMap,
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

    private void setSystemFieldPredefinedValueByCode(String code,
                                                     Long projectId,
                                                     Long organizationId,
                                                     ExcelColumnVO excelColumnVO,
                                                     boolean withFeature) {
        switch (code) {
            case FieldCode.PRIORITY:
                processPriority(organizationId, excelColumnVO);
                break;
            case FieldCode.ISSUE_TYPE:
                processIssueType(withFeature, projectId, excelColumnVO);
                break;
            case ExcelImportTemplate.IssueHeader.PARENT:
                processParentIssue(projectId, excelColumnVO);
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
            case FieldCode.ISSUE_STATUS:
                processIssueStatus(projectId, excelColumnVO);
                break;
            case FieldCode.PRODUCT:
                processIssueProduct(projectId, excelColumnVO);
                break;
            default:
                break;
        }
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

    protected void processIssueStatus(Long projectId, ExcelColumnVO excelColumnVO) {
        List<IssueTypeVO> issueTypes = projectConfigService.queryIssueTypesByProjectId(projectId, APPLY_TYPE_AGILE, true);
        Map<String, StatusVO> issueStatusMap = new HashMap<>();
        if (CollectionUtils.isEmpty(issueTypes)) {
            return;
        }
        issueTypes.forEach(issueTypeVO -> {
            List<StatusVO> issueStatusList = projectConfigService.queryStatusByIssueTypeId(projectId, issueTypeVO.getId(), APPLY_TYPE_AGILE);
            if (CollectionUtils.isEmpty(issueStatusList)) {
                return;
            }
            String typeCode = issueTypeVO.getTypeCode();
            issueStatusList.forEach(status -> {
                String key = issueTypeVO.getName() + "-" + status.getName();
                issueStatusMap.put(key, status);
                if (IssueTypeCode.BUG.value().equals(typeCode)) {
                    String subBugKey = SUB_BUG_CN + "-" + status.getName();
                    issueStatusMap.put(subBugKey, status);
                }
            });
        });
        excelColumnVO.setIssueStatusMap(issueStatusMap);
    }

    private void processEnvironment(ExcelColumnVO excelColumnVO, Long projectId) {
        List<String> values = Arrays.asList("非生产环境", "生产环境");
        excelColumnVO.setPredefinedValues(values);
        LookupTypeWithValuesVO environment = lookupValueService.queryLookupValueByCode("environment", projectId);
        excelColumnVO.setEnvNameCodeMap(environment.getLookupValues().stream().collect(Collectors.toMap(LookupValueVO::getName, LookupValueVO::getValueCode)));
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

    private void processUser(Long projectId, ExcelColumnVO excelColumnVO) {
        Map<String, Long> map = getManagers(projectId);
        List<String> values = new ArrayList<>(map.keySet());
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

    private void processParentIssue(Long projectId, ExcelColumnVO excelColumnVO) {
        List<IssueVO> issues = issueMapper.listUndoneAvailableParents(projectId);
        List<String> values = new ArrayList<>();
        Map<String, Long> map = new HashMap<>();
        issues.forEach(i -> {
            String issueNum = i.getIssueNum();
            values.add(issueNum);
            map.put(issueNum, i.getIssueId());
        });
        excelColumnVO.setPredefinedValues(values);
        excelColumnVO.setValueIdMap(map);
    }

    private void processIssueType(boolean withFeature, Long projectId, ExcelColumnVO excelColumnVO) {
        List<IssueTypeVO> issueTypes = projectConfigService.queryIssueTypesByProjectId(projectId, APPLY_TYPE_AGILE, true);
        List<String> values = new ArrayList<>();
        Map<String, IssueTypeVO> issueTypeMap = new HashMap<>();
        issueTypes.forEach(i -> {
            String typeCode = i.getTypeCode();
            String typeName = i.getName();
            issueTypeMap.put(typeName, i);
            if (withFeature && ISSUE_EPIC.equals(typeCode)) {
                return;
            }
            if (!FEATURE.equals(typeCode)) {
                values.add(typeName);
            }
            if ("bug".equals(typeCode)) {
                values.add(SUB_BUG_CN);
                issueTypeMap.put(SUB_BUG_CN, i);
            }
        });
        excelColumnVO.setPredefinedValues(values);
        excelColumnVO.setIssueTypeMap(issueTypeMap);
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

    protected Map<Integer, Integer> getSonParentMap(Map<Integer, Set<Integer>> parentSonMap) {
        Map<Integer, Integer> map = new HashMap<>();
        for (Map.Entry<Integer, Set<Integer>> entry : parentSonMap.entrySet()) {
            Integer key = entry.getKey();
            Set<Integer> value = entry.getValue();
            value.forEach(i -> map.put(i, key));
        }
        return map;
    }

    protected Map<Integer, Set<Integer>> getParentSonMap(List<IssueTypeLinkDTO> issueTypeLinks,
                                                         Map<Integer, ExcelColumnVO> headerMap) {
        Map<Integer, Set<Integer>> map = new HashMap<>();
        for (IssueTypeLinkDTO issueTypeLink : issueTypeLinks) {
            Integer rowNum = issueTypeLink.getRow();
            String type = issueTypeLink.getType();
            String issueTypeCode = getIssueTypeCode(headerMap, type);
            //故事和任务下有子任务子缺陷
            //缺陷下只有子任务，但保留子缺陷父子结构，后续有父子关系校验
            if (IssueTypeCode.isStory(issueTypeCode)
                    || IssueTypeCode.isTask(issueTypeCode)
                    || (IssueTypeCode.isBug(issueTypeCode) && !SUB_BUG_CN.equals(type))) {
                parentRecursive(map, issueTypeLink, rowNum, headerMap);
            }
        }
        return map;
    }

    private void processSonRow(Map<Integer, Set<Integer>> map, Integer rowNum, Integer nextRowNum) {
        Set<Integer> set = map.get(rowNum);
        if (set == null) {
            set = new HashSet<>();
            set.add(nextRowNum);
            map.put(rowNum, set);
        } else {
            set.add(nextRowNum);
        }
    }

    private void parentRecursive(Map<Integer, Set<Integer>> map,
                                IssueTypeLinkDTO issueTypeLink,
                                Integer rowNum,
                                Map<Integer, ExcelColumnVO> headerMap) {
        if (Boolean.TRUE.equals(issueTypeLink.hasNext())) {
            IssueTypeLinkDTO next = issueTypeLink.getNext();
            String nextType = next.getType();
            String nextIssueTypeCode = getIssueTypeCode(headerMap, nextType);
            Integer nextRowNum = next.getRow();
            if (IssueTypeCode.isSubTask(nextIssueTypeCode)
                    || SUB_BUG_CN.equals(nextType)) {
                processSonRow(map, rowNum, nextRowNum);
                parentRecursive(map, next, rowNum, headerMap);
            }
        }
    }

    protected void validateWorkbook(Workbook workbook, FileOperationHistoryDTO history, String websocketKey) {
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

    protected List<String> resolveCodeFromHeader(Workbook workbook,
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

    @Override
    public FileOperationHistoryDTO initFileOperationHistory(Long projectId,
                                                            Long userId,
                                                            String status,
                                                            String action,
                                                            String websocketKey) {
        FileOperationHistoryDTO fileOperationHistoryDTO = new FileOperationHistoryDTO(projectId, userId, action, 0L, 0L, status);
        if (fileOperationHistoryMapper.insertSelective(fileOperationHistoryDTO) != 1) {
            throw new CommonException("error.FileOperationHistoryDTO.insert");
        }
        FileOperationHistoryDTO res = fileOperationHistoryMapper.selectByPrimaryKey(fileOperationHistoryDTO.getId());
        sendProcess(res, userId, 0.0, websocketKey);
        return res;
    }

    @Override
    public FileOperationHistoryDTO initFileOperationHistory(Long projectId, Long organizationId, Long userId, String status, String action, String websocketKey) {
        FileOperationHistoryDTO fileOperationHistoryDTO = new FileOperationHistoryDTO(projectId, organizationId, userId, action, 0L, 0L, status);
        if (fileOperationHistoryMapper.insertSelective(fileOperationHistoryDTO) != 1) {
            throw new CommonException("error.FileOperationHistoryDTO.insert");
        }
        FileOperationHistoryDTO res = fileOperationHistoryMapper.selectByPrimaryKey(fileOperationHistoryDTO.getId());
        sendProcess(res, userId, 0.0, websocketKey);
        return res;
    }


    @Override
    public void cancelImport(Long projectId, Long id, Long objectVersionNumber) {
        FileOperationHistoryDTO fileOperationHistoryDTO = new FileOperationHistoryDTO();
        fileOperationHistoryDTO.setId(id);
        fileOperationHistoryDTO.setStatus(CANCELED);
        fileOperationHistoryDTO.setObjectVersionNumber(objectVersionNumber);
        if (fileOperationHistoryMapper.updateByPrimaryKeySelective(fileOperationHistoryDTO) != 1) {
            throw new CommonException(ERROR_FILE_OPERATION_HISTORY_UPDATE);
        }
    }


    @Override
    public FileOperationHistoryVO queryLatestRecode(Long projectId, String action) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        FileOperationHistoryDTO result = fileOperationHistoryMapper.queryLatestRecode(projectId, userId, action);
        return result == null ? new FileOperationHistoryVO() : modelMapper.map(result, FileOperationHistoryVO.class);
    }

    @Override
    @Async
    public void asyncExportIssues(Long projectId,
                                  SearchVO searchVO,
                                  HttpServletRequest request,
                                  HttpServletResponse response,
                                  Long organizationId,
                                  Sort sort,
                                  ServletRequestAttributes requestAttributes) {
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String websocketKey = WEBSOCKET_EXPORT_CODE + "-" + projectId;
        FileOperationHistoryDTO fileOperationHistoryDTO = initFileOperationHistory(projectId, userId, DOING, DOWNLOAD_FILE, websocketKey);
        //处理根据界面筛选结果导出的字段
        Map<String, String[]> fieldMap =
                handleExportFields(searchVO.getExportFieldCodes(), projectId, organizationId, FIELDS_NAMES, FIELDS);
        String[] fieldCodes = sortFieldCodes(fieldMap.get(FIELD_CODES));
        String[] fieldNames = sortFieldNames(fieldMap.get(FIELD_NAMES));
        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
        projectInfoDTO.setProjectId(projectId);
        projectInfoDTO = projectInfoMapper.selectOne(projectInfoDTO);
        ProjectVO project = userService.queryProject(projectId);
        if (project == null) {
            throw new CommonException(PROJECT_ERROR);
        }
        project.setCode(projectInfoDTO.getProjectCode());
        Boolean condition = issueService.handleSearchUser(searchVO, projectId);
        boolean isTreeView =
                !Boolean.FALSE.equals(
                        Optional.ofNullable(searchVO.getSearchArgs())
                                .map(x -> x.get("tree"))
                                .orElse(false));

        String sheetName = project.getName();
        Workbook workbook = ExcelUtil.initIssueExportWorkbook(sheetName, fieldNames);
        ExcelCursorDTO cursor = new ExcelCursorDTO(1, 0, 1000);
        if (Boolean.TRUE.equals(condition)) {
            String filterSql = null;
            if (searchVO.getQuickFilterIds() != null && !searchVO.getQuickFilterIds().isEmpty()) {
                filterSql = getQuickFilter(searchVO.getQuickFilterIds());
            }
            final String searchSql = filterSql;
            double lastProcess = 0D;
            PageRequest sourcePage = new PageRequest(0, 1, sort);
            Map<String, Object> sortMap = issueService.processSortMap(sourcePage, projectId, organizationId);
            while (true) {
                //查询所有父节点问题
                PageRequest pageRequest = new PageRequest(cursor.getPage(), cursor.getSize());
                Page<Long> page = issueService.pagedQueryByTreeView(pageRequest, new HashSet<>(Arrays.asList(projectId)), searchVO, searchSql, sortMap, isTreeView);
                if (CollectionUtils.isEmpty(page.getContent())) {
                    break;
                }
                List<Long> parentIds = page.getContent();
                List<Long> issueIds = new ArrayList<>();
                Map<Long, Set<Long>> parentSonMap = new HashMap<>();
                List<IssueDTO> issues = new ArrayList<>();
                if (!parentIds.isEmpty()) {
                    Set<Long> childrenIds = new HashSet<>();
                    if (isTreeView) {
                        List<IssueDTO> childIssues = issueMapper.queryChildrenIdByParentId(parentIds, new HashSet<>(Arrays.asList(projectId)), searchVO, searchSql, searchVO.getAssigneeFilterIds(), null);
                        childrenIds.addAll(childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toSet()));
                    }
                    cursor.addCollections(childrenIds);
                    issues = issueMapper.queryIssueListWithSubByIssueIds(parentIds, childrenIds, true, isTreeView);
                }
                Map<Long, ExportIssuesVO> issueMap = new LinkedHashMap<>();
                cursor
                        .addCollections(page.getContent())
                        .addCollections(parentIds)
                        .addCollections(issueIds)
                        .addCollections(parentSonMap)
                        .addCollections(issueMap)
                        .addCollections(issues);
                if (!ObjectUtils.isEmpty(issues)) {
                    Set<Long> userIds = new HashSet<>();
                    issues.forEach(i -> {
                        issueIds.add(i.getIssueId());
                        Long assigneeId = i.getAssigneeId();
                        Long reporterId = i.getReporterId();
                        Long createdUser = i.getCreatedBy();
                        Long updatedUser = i.getLastUpdatedBy();
                        Long mainResponsibleId = i.getMainResponsibleId();
                        if (!ObjectUtils.isEmpty(assigneeId) && !Objects.equals(assigneeId, 0L)) {
                            userIds.add(assigneeId);
                        }
                        if (!ObjectUtils.isEmpty(reporterId) && !Objects.equals(reporterId, 0L)) {
                            userIds.add(reporterId);
                        }
                        if (!ObjectUtils.isEmpty(createdUser) && !Objects.equals(createdUser, 0L)) {
                            userIds.add(createdUser);
                        }
                        if (!ObjectUtils.isEmpty(updatedUser) && !Objects.equals(updatedUser, 0L)) {
                            userIds.add(updatedUser);
                        }
                        if (!ObjectUtils.isEmpty(mainResponsibleId) && !Objects.equals(mainResponsibleId, 0L)) {
                            userIds.add(mainResponsibleId);
                        }
                        if (!CollectionUtils.isEmpty(i.getParticipantIds())) {
                            userIds.addAll(i.getParticipantIds());
                        }
                    });
                    Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(ListUtil.filterByKey(new ArrayList<>(userIds), 0L), true);
                    Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
                    Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
                    Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
                    Map<Long, List<SprintNameDTO>> closeSprintNames = issueMapper.querySprintNameByIssueIds(Arrays.asList(projectId), issueIds).stream().collect(Collectors.groupingBy(SprintNameDTO::getIssueId));
                    Map<Long, List<VersionIssueRelDTO>> fixVersionNames = issueMapper.queryVersionNameByIssueIds(Arrays.asList(projectId), issueIds, FIX_RELATION_TYPE).stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
                    Map<Long, List<VersionIssueRelDTO>> influenceVersionNames = issueMapper.queryVersionNameByIssueIds(Arrays.asList(projectId), issueIds, INFLUENCE_RELATION_TYPE).stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
                    Map<Long, List<LabelIssueRelDTO>> labelNames = issueMapper.queryLabelIssueByIssueIds(Arrays.asList(projectId), issueIds).stream().collect(Collectors.groupingBy(LabelIssueRelDTO::getIssueId));
                    Map<Long, List<ComponentIssueRelDTO>> componentMap = issueMapper.queryComponentIssueByIssueIds(Arrays.asList(projectId), issueIds).stream().collect(Collectors.groupingBy(ComponentIssueRelDTO::getIssueId));
                    Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, Arrays.asList(projectId), issueIds, true);
                    Map<Long, List<WorkLogVO>> workLogVOMap = workLogMapper.queryByIssueIds(Collections.singletonList(projectId), issueIds).stream().collect(Collectors.groupingBy(WorkLogVO::getIssueId));
                    Map<String, String> envMap = lookupValueService.queryMapByTypeCode(FieldCode.ENVIRONMENT);
                    Map<Long, Set<TagVO>> tagMap = new HashMap<>();
                    Map<Long, List<ProductVO>> productMap = new HashMap<>();
                    if (agilePluginService != null) {
                        tagMap.putAll(agilePluginService.listTagMap(ConvertUtil.getOrganizationId(projectId), new HashSet<>(Arrays.asList(projectId)), issueIds));
                        productMap.putAll(agilePluginService.listProductMap(ConvertUtil.getOrganizationId(projectId), Arrays.asList(projectId), issueIds));
                    }
                    Map<Long, List<IssueLinkDTO>> relatedIssueMap =
                            issueLinkMapper.queryIssueLinkByIssueId(new HashSet<>(issueIds), new HashSet<>(Arrays.asList(projectId)), false)
                                    .stream()
                                    .collect(Collectors.groupingBy(IssueLinkDTO::getKeyIssueId));
                    cursor.addCollections(userIds);
                    Map<String, Object> issueValueMap = new HashMap<>();
                    issueValueMap.put(USER_MAP, usersMap);
                    issueValueMap.put(ISSUE_TYPE_MAP, issueTypeDTOMap);
                    issueValueMap.put(STATUS_MAP, statusMapDTOMap);
                    issueValueMap.put(PRIORITY_MAP, priorityDTOMap);
                    issueValueMap.put(CLOSE_SPRINT_MAP, closeSprintNames);
                    issueValueMap.put(FIX_VERSION_MAP, fixVersionNames);
                    issueValueMap.put(INFLUENCE_VERSION_MAP, influenceVersionNames);
                    issueValueMap.put(LABEL_MAP, labelNames);
                    issueValueMap.put(COMPONENT_MAP, componentMap);
                    issueValueMap.put(FOUNDATION_CODE_VALUE_MAP, foundationCodeValue);
                    issueValueMap.put(ENV_MAP, envMap);
                    issueValueMap.put(WORK_LOG_MAP, workLogVOMap);
                    issueValueMap.put(TAG_MAP, tagMap);
                    issueValueMap.put(RELATED_ISSUE_MAP, relatedIssueMap);
                    issueValueMap.put(PRODUCT_MAP, productMap);
                    issueValueMap.forEach((k, v) -> cursor.addCollections(v));
                    issues.forEach(issue ->
                            buildExcelIssueFromIssue(
                                    project.getName(),
                                    parentSonMap,
                                    issueMap,
                                    issueValueMap,
                                    issue));
                }
                if (!isTreeView) {
                    parentSonMap.clear();
                }
                ExcelUtil.writeIssue(issueMap, parentSonMap, ExportIssuesVO.class, fieldNames, fieldCodes, sheetName, Arrays.asList(AUTO_SIZE_WIDTH), workbook, cursor);
                boolean hasNextPage = (cursor.getPage() + 1) < page.getTotalPages();
                cursor.clean();
                double process = getProcess(cursor.getPage(), page.getTotalPages());
                if (process - lastProcess >= 0.1) {
                    sendProcess(fileOperationHistoryDTO, userId, process, websocketKey);
                    lastProcess = process;
                }
                if (!hasNextPage) {
                    break;
                }
                //查询后页数增1
                cursor.increasePage();
            }
        }
        String fileName = project.getName() + FILESUFFIX;
        //把workbook上传到对象存储服务中
        downloadWorkBook(organizationId, workbook, fileName, fileOperationHistoryDTO, userId);
    }

    @Override
    public ExportIssuesVO buildExcelIssueFromIssue(String projectName,
                                                      Map<Long, Set<Long>> parentSonMap,
                                                      Map<Long, ExportIssuesVO> issueMap,
                                                      Map<String, Object> issueValueMap,
                                                      IssueDTO issue) {
        Map<Long, UserMessageDTO> usersMap = (Map<Long, UserMessageDTO>) issueValueMap.get(USER_MAP);
        Map<Long, IssueTypeVO> issueTypeDTOMap = (Map<Long, IssueTypeVO>) issueValueMap.get(ISSUE_TYPE_MAP);
        Map<Long, StatusVO> statusMapDTOMap = (Map<Long, StatusVO>) issueValueMap.get(STATUS_MAP);
        Map<Long, PriorityVO> priorityDTOMap = (Map<Long, PriorityVO>) issueValueMap.get(PRIORITY_MAP);
        Map<Long, List<SprintNameDTO>> closeSprintNames = (Map<Long, List<SprintNameDTO>>) issueValueMap.get(CLOSE_SPRINT_MAP);
        Map<Long, List<VersionIssueRelDTO>> fixVersionNames = (Map<Long, List<VersionIssueRelDTO>>) issueValueMap.get(FIX_VERSION_MAP);
        Map<Long, List<VersionIssueRelDTO>> influenceVersionNames = (Map<Long, List<VersionIssueRelDTO>>) issueValueMap.get(INFLUENCE_VERSION_MAP);
        Map<Long, List<LabelIssueRelDTO>> labelNames = (Map<Long, List<LabelIssueRelDTO>>) issueValueMap.get(LABEL_MAP);
        Map<Long, List<ComponentIssueRelDTO>> componentMap = (Map<Long, List<ComponentIssueRelDTO>>) issueValueMap.get(COMPONENT_MAP);
        Map<Long, Map<String, Object>> foundationCodeValue = (Map<Long, Map<String, Object>>) issueValueMap.get(FOUNDATION_CODE_VALUE_MAP);
        Map<String, String> envMap = (Map<String, String>) issueValueMap.get(ENV_MAP);
        Map<Long, List<WorkLogVO>> workLogVOMap = (Map<Long, List<WorkLogVO>>) issueValueMap.get(WORK_LOG_MAP);
        Map<Long, Set<TagVO>> tagMap = (Map<Long, Set<TagVO>>) issueValueMap.get(TAG_MAP);
        Map<Long, List<IssueLinkDTO>> relatedIssueMap = (Map<Long, List<IssueLinkDTO>>) issueValueMap.get(RELATED_ISSUE_MAP);
        Map<Long, List<ProductVO>> productMap = (Map<Long, List<ProductVO>>) issueValueMap.get(PRODUCT_MAP);
        Long issueId = issue.getIssueId();
        ExportIssuesVO exportIssuesVO = new ExportIssuesVO();
        BeanUtils.copyProperties(issue, exportIssuesVO);

        exportIssuesVO.setProjectName(projectName);
        exportIssuesVO.setSprintName(getActiveSprintName(issue));
        setAssignee(usersMap, issue, exportIssuesVO);
        serReporter(usersMap, issue, exportIssuesVO);
        setMainResponsible(usersMap, issue, exportIssuesVO);
        setEnvironmentName(envMap, issue, exportIssuesVO);
        setPriorityName(priorityDTOMap, issue, exportIssuesVO);
        setStatusName(statusMapDTOMap, issue, exportIssuesVO);
        if (!ObjectUtils.isEmpty(issueTypeDTOMap)) {
            setTypeName(issueTypeDTOMap, issue, exportIssuesVO);
        }
        setCloseSprintName(closeSprintNames, issueId, exportIssuesVO);
        setFixVersionName(fixVersionNames, issueId, exportIssuesVO);
        setCreationUserName(usersMap, issue, exportIssuesVO);
        setLastUpdatedUserName(usersMap, issue, exportIssuesVO);
        exportIssuesVO.setSprintName(exportIssuesSprintName(exportIssuesVO));
        setInfluenceVersionName(influenceVersionNames, issueId, exportIssuesVO);
        setLabelName(labelNames, issueId, exportIssuesVO);
        setComponentName(componentMap, issueId, exportIssuesVO);
        exportIssuesVO.setVersionName(exportIssuesVersionName(exportIssuesVO));
        exportIssuesVO.setDescription(getDes(exportIssuesVO.getDescription()));
        setFoundationFieldValue(foundationCodeValue, issueId, exportIssuesVO);
        issueMap.put(issueId, exportIssuesVO);
        processParentSonRelation(parentSonMap, issue);
        setSpentWorkTimeAndAllEstimateTime(workLogVOMap, exportIssuesVO);
        resetRemainingTimeIfCompleted(issue, exportIssuesVO);
        setTag(tagMap, exportIssuesVO);
        setRelatedIssue(exportIssuesVO, relatedIssueMap);
        setParticipant(exportIssuesVO, issue, usersMap);
        setProduct(exportIssuesVO, issue, productMap);
        return exportIssuesVO;
    }

    private void setProduct(ExportIssuesVO exportIssuesVO, IssueDTO issue, Map<Long, List<ProductVO>> productMap) {
        List<ProductVO> productVOList = productMap.get(issue.getIssueId());
        String productNames = "";
        if (!ObjectUtils.isEmpty(productVOList)) {
            productNames = productVOList.stream().map(ProductVO::getName).collect(Collectors.joining(","));
        }
        exportIssuesVO.setProduct(productNames);
    }

    private void setParticipant(ExportIssuesVO exportIssuesVO, IssueDTO issue, Map<Long, UserMessageDTO> usersMap) {
        List<Long> participantIds = issue.getParticipantIds();
        if (!CollectionUtils.isEmpty(participantIds)) {
            List<String> participants = new ArrayList<>();
            for (Long participantId : participantIds) {
                UserMessageDTO userMessageDTO = usersMap.get(participantId);
                if (!ObjectUtils.isEmpty(userMessageDTO)) {
                    participants.add(userMessageDTO.getName());
                }
            }
            exportIssuesVO.setParticipant(participants.stream().collect(Collectors.joining(",")));
        }
    }

    private void setRelatedIssue(ExportIssuesVO exportIssuesVO, Map<Long, List<IssueLinkDTO>> relatedIssueMap) {
        Long issueId = exportIssuesVO.getIssueId();
        List<IssueLinkDTO> issueLinkList = relatedIssueMap.get(issueId);
        Map<String, List<String>> relMap = new LinkedHashMap<>();
        if(!ObjectUtils.isEmpty(issueLinkList)) {
            int size = issueLinkList.size();
            exportIssuesVO.setRelatedIssueCount(size);
            Iterator<IssueLinkDTO> iterator = issueLinkList.iterator();
            while (iterator.hasNext()) {
                IssueLinkDTO dto = iterator.next();
                String action;
                Long linkedIssueId = dto.getLinkedIssueId();
                if (issueId.equals(linkedIssueId)) {
                    action = dto.getWard();
                } else {
                    action = dto.getLinkTypeName();
                }
                String issueNum = dto.getIssueNum();
                String summary = dto.getSummary();
                List<String> rowList = relMap.computeIfAbsent(action, x -> new ArrayList<>());
                String row = action + COLON_CN + issueNum + COLON_CN + summary;
                rowList.add(row);
            }
        }
        List<String> rows = new ArrayList<>();
        relMap.forEach((k, v) -> rows.addAll(v));
        Iterator<String> rowIterator = rows.iterator();
        StringBuilder builder = new StringBuilder();
        while (rowIterator.hasNext()) {
            builder.append(rowIterator.next());
            if (rowIterator.hasNext()) {
                builder.append("\n");
            }
        }
        exportIssuesVO.setRelatedIssue(builder.toString());
    }

    private void setTag(Map<Long, Set<TagVO>> tagMap, ExportIssuesVO exportIssuesVO) {
        Long issueId = exportIssuesVO.getIssueId();
        Set<TagVO> tags = tagMap.get(issueId);
        if (!ObjectUtils.isEmpty(tags)) {
            List<TagVO> tagList = new ArrayList<>(tags);
            tagList.sort(Comparator.comparing(TagVO::getProjectId)
                    .thenComparing(TagVO::getAppServiceCode)
                    .thenComparing(TagVO::getTagName));
            StringBuilder build = new StringBuilder();
            Iterator<TagVO> iterator = tagList.iterator();
            while (iterator.hasNext()) {
                TagVO tag = iterator.next();
                build.append(tag.getAppServiceCode()).append(COLON_CN).append(tag.getTagName());
                if (iterator.hasNext()) {
                    build.append("，");
                }
            }
            exportIssuesVO.setTags(build.toString());
        }
    }

    /**
     * 设置预估时间和耗费时间
     *
     * @param workLogVOMap
     * @param exportIssuesVO
     */
    private void setSpentWorkTimeAndAllEstimateTime(Map<Long, List<WorkLogVO>> workLogVOMap, ExportIssuesVO exportIssuesVO) {
        List<WorkLogVO> workLogVOList = workLogVOMap.get(exportIssuesVO.getIssueId());
        BigDecimal spentWorkTime = null;
        BigDecimal allEstimateTime;
        if (!CollectionUtils.isEmpty(workLogVOList)) {
            spentWorkTime = new BigDecimal(0);
            for (WorkLogVO workLogVO : workLogVOList){
                spentWorkTime = spentWorkTime.add(workLogVO.getWorkTime());
            }
            allEstimateTime = exportIssuesVO.getRemainingTime() == null ? spentWorkTime : spentWorkTime.add(exportIssuesVO.getRemainingTime());
        } else {
            allEstimateTime = exportIssuesVO.getRemainingTime() == null ? new BigDecimal(0) : exportIssuesVO.getRemainingTime();
        }
        exportIssuesVO.setSpentWorkTime(spentWorkTime);
        exportIssuesVO.setAllEstimateTime(allEstimateTime);
    }

    private void setEnvironmentName(Map<String, String> envMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        String environment = issue.getEnvironment();
        if (!StringUtils.isEmpty(environment)) {
            exportIssuesVO.setEnvironmentName(envMap.get(environment));
        }
    }

    private void setMainResponsible(Map<Long, UserMessageDTO> usersMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        Long mainResponsibleId = issue.getMainResponsibleId();
        UserMessageDTO userMessage = usersMap.get(mainResponsibleId);
        if (!ObjectUtils.isEmpty(userMessage)) {
            exportIssuesVO.setMainResponsibleName(userMessage.getName());
        }
    }

    private void resetRemainingTimeIfCompleted(IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        if (Boolean.TRUE.equals(issue.getCompleted())) {
            exportIssuesVO.setRemainingTime(new BigDecimal(0));
        }
    }

    protected Double getProcess(Integer currentNum, Integer totalNum) {
        double process = (currentNum + 1.0) / (totalNum + 1.0) * 0.95 * 100;
        BigDecimal b = BigDecimal.valueOf(process);
        process = b.setScale(1, BigDecimal.ROUND_HALF_UP).doubleValue();
        return process;
    }

    /**
     * 上传文件到minio中
     *
     * @param organizationId
     * @param workbook
     * @param fileName
     * @param fileOperationHistoryDTO
     * @param userId
     */
    @Override
    public void downloadWorkBook(Long organizationId,Workbook workbook, String fileName, FileOperationHistoryDTO fileOperationHistoryDTO, Long userId) {
        try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
            workbook.write(os);
            byte[] content = os.toByteArray();
            MultipartFile file = new MultipartExcel("file", fileName, EXCELCONTENTTYPE, content);

            //返回上载结果
            String path = fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(),null, fileName, file);
            fileOperationHistoryDTO.setStatus(SUCCESS);
            fileOperationHistoryDTO.setFileUrl(path);
        } catch (Exception e) {
            fileOperationHistoryDTO.setStatus(FAILED);
            LOGGER.error("upload file error: {}", e);
        } finally {
            try {
                fileOperationHistoryDTO.setLastUpdateDate(new Date());
                fileOperationHistoryMapper.updateByPrimaryKey(fileOperationHistoryDTO);
                String websocketKey = "";
                switch (fileOperationHistoryDTO.getAction()) {
                    case DOWNLOAD_FILE:
                        websocketKey = WEBSOCKET_EXPORT_CODE + "-" + fileOperationHistoryDTO.getProjectId();
                        break;
                    case DOWNLOAD_FILE_PUBLISH_VERSION:
                        websocketKey = WEBSOCKET_EXPORT_PUBLISH_VERSION + "-" + fileOperationHistoryDTO.getProjectId();
                        break;
                    default:
                        break;
                }
                sendProcess(fileOperationHistoryDTO, userId, 100.0, websocketKey);
                if (workbook instanceof SXSSFWorkbook) {
                    //处理在磁盘上支持本工作簿的临时文件
                    ((SXSSFWorkbook) workbook).dispose();
                }
                workbook.close();
            } catch (IOException e) {
                LOGGER.warn(EXPORT_ERROR_WORKBOOK_CLOSE, e);
            }
        }
    }


    protected void setLabelName(Map<Long, List<LabelIssueRelDTO>> labelNames, Long issueId, ExportIssuesVO exportIssuesVO) {
        String labelName = "";
        List<LabelIssueRelDTO> labelIssueRel = labelNames.get(issueId);
        if (!ObjectUtils.isEmpty(labelIssueRel)) {
            labelName = labelIssueRel.stream().map(LabelIssueRelDTO::getLabelName).collect(Collectors.joining(","));
        }
        exportIssuesVO.setLabelName(labelName);
    }

    protected void setComponentName(Map<Long, List<ComponentIssueRelDTO>> componentMap, Long issueId, ExportIssuesVO exportIssuesVO) {
        String componentName = "";
        List<ComponentIssueRelDTO> componentIssueRel = componentMap.get(issueId);
        if (!ObjectUtils.isEmpty(componentIssueRel)) {
            componentName = componentIssueRel.stream().map(ComponentIssueRelDTO::getName).collect(Collectors.joining(","));
        }
        exportIssuesVO.setComponentName(componentName);
    }

    protected void setFoundationFieldValue(Map<Long, Map<String, Object>> foundationCodeValue, Long issueId, ExportIssuesVO exportIssuesVO) {
        Map<String, Object> fieldValue = foundationCodeValue.get(issueId);
        if (fieldValue == null) {
            fieldValue = new HashMap<>();
        }
        exportIssuesVO.setFoundationFieldValue(fieldValue);
    }

    protected void setInfluenceVersionName(Map<Long, List<VersionIssueRelDTO>> influenceVersionNames, Long issueId, ExportIssuesVO exportIssuesVO) {
        String influenceVersionName = "";
        List<VersionIssueRelDTO> versionIssueRel = influenceVersionNames.get(issueId);
        if (!ObjectUtils.isEmpty(versionIssueRel)) {
            influenceVersionName = versionIssueRel.stream().map(VersionIssueRelDTO::getName).collect(Collectors.joining(","));
        }
        exportIssuesVO.setInfluenceVersionName(influenceVersionName);
    }

    protected void setCloseSprintName(Map<Long, List<SprintNameDTO>> closeSprintNames, Long issueId, ExportIssuesVO exportIssuesVO) {
        String closeSprintName = "";
        List<SprintNameDTO> sprintNames = closeSprintNames.get(issueId);
        if (!ObjectUtils.isEmpty(sprintNames)) {
            closeSprintName =
                    sprintNames
                            .stream()
                            .map(SprintNameDTO::getSprintName)
                            .collect(Collectors.joining(","));
        }
        exportIssuesVO.setCloseSprintName(closeSprintName);
    }

    protected void setFixVersionName(Map<Long, List<VersionIssueRelDTO>> fixVersionNames, Long issueId, ExportIssuesVO exportIssuesVO) {
        String fixVersionName = "";
        List<VersionIssueRelDTO> versionIssueRel = fixVersionNames.get(issueId);
        if (!ObjectUtils.isEmpty(versionIssueRel)) {
            fixVersionName =
                    versionIssueRel
                            .stream()
                            .map(VersionIssueRelDTO::getName)
                            .collect(Collectors.joining(","));
        }
        exportIssuesVO.setFixVersionName(fixVersionName);
    }

    protected void setTypeName(Map<Long, IssueTypeVO> issueTypeDTOMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        IssueTypeVO issueTypeVO = issueTypeDTOMap.get(issue.getIssueTypeId());
        if (!ObjectUtils.isEmpty(issueTypeVO)) {
            exportIssuesVO.setTypeName(issueTypeVO.getName());
        }
    }

    protected void setStatusName(Map<Long, StatusVO> statusMapDTOMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        StatusVO statusVO = statusMapDTOMap.get(issue.getStatusId());
        if (!ObjectUtils.isEmpty(statusVO)) {
            exportIssuesVO.setStatusName(statusVO.getName());
        }
    }

    protected void setPriorityName(Map<Long, PriorityVO> priorityDTOMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        Long priorityId = issue.getPriorityId();
        PriorityVO priorityVO = priorityDTOMap.get(priorityId);
        if (!ObjectUtils.isEmpty(priorityVO)) {
            exportIssuesVO.setPriorityName(priorityVO.getName());
        }
    }

    protected void serReporter(Map<Long, UserMessageDTO> usersMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        Long reporterId = issue.getReporterId();
        UserMessageDTO userMessage = usersMap.get(reporterId);
        if (!ObjectUtils.isEmpty(userMessage)) {
            exportIssuesVO.setReporterName(userMessage.getName());
            exportIssuesVO.setReporterRealName(userMessage.getRealName());
        }
    }

    protected void setAssignee(Map<Long, UserMessageDTO> usersMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        Long assigneeId = issue.getAssigneeId();
        UserMessageDTO userMessage = usersMap.get(assigneeId);
        if (!ObjectUtils.isEmpty(userMessage)) {
            exportIssuesVO.setAssigneeName(userMessage.getName());
            exportIssuesVO.setAssigneeRealName(userMessage.getRealName());
        }
    }

    protected void setCreationUserName(Map<Long, UserMessageDTO> usersMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        Long createdUser = issue.getCreatedBy();
        UserMessageDTO userMessage = usersMap.get(createdUser);
        if (!ObjectUtils.isEmpty(userMessage)) {
            exportIssuesVO.setCreatedUserName(userMessage.getName());
            exportIssuesVO.setCreatedUserRealName(userMessage.getRealName());
        }
    }

    protected void setLastUpdatedUserName(Map<Long, UserMessageDTO> usersMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
        Long updatedUser = issue.getLastUpdatedBy();
        UserMessageDTO userMessage = usersMap.get(updatedUser);
        if (!ObjectUtils.isEmpty(userMessage)) {
            exportIssuesVO.setLastUpdatedUserName(userMessage.getName());
            exportIssuesVO.setLastUpdatedUserRealName(userMessage.getRealName());
        }
    }

    protected String getActiveSprintName(IssueDTO issue) {
        List<IssueSprintDTO>  issueSprintList = issue.getIssueSprintDTOS();
        if (!ObjectUtils.isEmpty(issueSprintList)) {
            for(IssueSprintDTO sprint : issueSprintList) {
                if (!"closed".equals(sprint.getStatusCode())) {
                    return sprint.getSprintName();
                }
            }
        }
        return null;
    }

    protected String[] sortFieldNames(String[] fieldNames) {
        List<String> result = new ArrayList<>();
        result.add(IssueConstant.ISSUE_TYPE_CN);
        result.add("概要");
        result.add(IssueConstant.ISSUE_CN + "编号");
        for (String str : fieldNames) {
            if (result.get(0).equals(str)
                    || result.get(1).equals(str)
                    || result.get(2).equals(str)) {
                continue;
            }
            result.add(str);
        }
        return result.toArray(new String[result.size()]);
    }

    protected String[] sortFieldCodes(String[] fieldCodes) {
        List<String> result = new ArrayList<>();
        result.add(TYPE_NAME);
        result.add(SUMMARY);
        result.add(ISSUE_NUM);
        for (String str : fieldCodes) {
            if (result.get(0).equals(str)
                    || result.get(1).equals(str)
                    || result.get(2).equals(str)) {
                continue;
            }
            result.add(str);
        }
        return result.toArray(new String[result.size()]);
    }

    protected void processParentSonRelation(Map<Long, Set<Long>> parentSonMap, IssueDTO issue) {
        String typeCode = issue.getTypeCode();
        Long issueId = issue.getIssueId();
        if (IssueTypeCode.isBug(typeCode)) {
            Long relateIssueId = issue.getRelateIssueId();
            if (!ObjectUtils.isEmpty(relateIssueId) && !Objects.equals(relateIssueId, 0L)) {
                appendToParentSonMap(relateIssueId, issueId, parentSonMap);
            }
        }
        if (IssueTypeCode.isSubTask(typeCode)) {
            Long parentIssueId = issue.getParentIssueId();
            if (!ObjectUtils.isEmpty(parentIssueId) && !Objects.equals(parentIssueId, 0L)) {
                appendToParentSonMap(parentIssueId, issueId, parentSonMap);
            }
        }
    }

    private void appendToParentSonMap(Long parentId, Long issueId, Map<Long, Set<Long>> parentSonMap) {
        Set<Long> childrenSet = parentSonMap.computeIfAbsent(parentId, k -> new HashSet<>());
        childrenSet.add(issueId);
    }

    protected String getQuickFilter(List<Long> quickFilterIds) {
        List<String> sqlQuerys = quickFilterMapper.selectSqlQueryByIds(quickFilterIds);
        if (sqlQuerys.isEmpty()) {
            return null;
        }
        int idx = 0;
        StringBuilder sql = new StringBuilder("select issue_id from agile_issue where ");
        for (String filter : sqlQuerys) {
            if (idx == 0) {
                sql.append(" ( " + filter + " ) ");
                idx += 1;
            } else {
                sql.append(" and " + " ( " + filter + " ) ");
            }
        }
        return sql.toString();
    }

    /**
     * 处理根据界面筛选结果导出的字段
     *
     * @param exportFieldCodes
     * @return
     */
    protected Map<String, String[]> handleExportFields(List<String> exportFieldCodes,
                                                       Long projectId,
                                                       Long organizationId,
                                                       String[] fieldsName,
                                                       String[] fields) {
        Map<String, String[]> fieldMap = new HashMap<>(2);
        ObjectMapper m = new ObjectMapper();

        Object content = Optional.ofNullable(objectSchemeFieldService
                .listQuery(organizationId, projectId, ObjectSchemeCode.AGILE_ISSUE))
                .orElseThrow(() -> new CommonException("error.foundation.listQuery"))
                .get("content");

        processExportField(exportFieldCodes, fieldsName, fields, fieldMap, m, content);
        return fieldMap;
    }

    @Override
    public void processExportField(List<String> exportFieldCodes,
                                    String[] fieldsName,
                                    String[] fields,
                                    Map<String, String[]> fieldMap,
                                    ObjectMapper m,
                                    Object content) {
        List<Object> contentList = m.convertValue(content, List.class);
        List<ObjectSchemeFieldDTO> fieldDTOS = new ArrayList<>();

        if (content != null) {
            contentList.forEach(k ->
                    fieldDTOS.add(m.convertValue(k, ObjectSchemeFieldDTO.class)));
        }

        List<ObjectSchemeFieldDTO> userDefinedFieldDTOS = fieldDTOS.stream().
                filter(v -> !v.getSystem()).collect(Collectors.toList());

        if (exportFieldCodes != null && !exportFieldCodes.isEmpty()) {
            Map<String, String> data = new HashMap<>(fields.length + userDefinedFieldDTOS.size());
            for (int i = 0; i < fields.length; i++) {
                data.put(fields[i], fieldsName[i]);
            }
            for (ObjectSchemeFieldDTO userDefinedFieldDTO : userDefinedFieldDTOS) {
                data.put(userDefinedFieldDTO.getCode(), userDefinedFieldDTO.getName());
            }

            List<String> fieldCodes = new ArrayList<>(exportFieldCodes.size());
            List<String> fieldNames = new ArrayList<>(exportFieldCodes.size());
            exportFieldCodes.forEach(code -> {
                String name = data.get(code);
                if (name != null) {
                    fieldCodes.add(code);
                    fieldNames.add(name);
                } else {
                    throw new CommonException("error.issue.illegal.exportField", code);
                }
            });
            fieldMap.put(FIELD_CODES, fieldCodes.stream().toArray(String[]::new));
            fieldMap.put(FIELD_NAMES, fieldNames.stream().toArray(String[]::new));
        } else {
            if (!userDefinedFieldDTOS.isEmpty()) {
                List<String> fieldCodes = new ArrayList(Arrays.asList(fields));
                List<String> fieldNames = new ArrayList(Arrays.asList(fieldsName));
                userDefinedFieldDTOS.forEach(fieldDTO -> {
                    fieldCodes.add(fieldDTO.getCode());
                    fieldNames.add(fieldDTO.getName());
                });

                fieldMap.put(FIELD_CODES, fieldCodes.stream().toArray(String[]::new));
                fieldMap.put(FIELD_NAMES, fieldNames.stream().toArray(String[]::new));
            } else {
                fieldMap.put(FIELD_CODES, fields);
                fieldMap.put(FIELD_NAMES, fieldsName);
            }
        }
    }

    public String getDes(String str) {
        if (str == null) {
            return "";
        }
        StringBuilder result = new StringBuilder();
        try {
            JSONArray root = JSON.parseArray(str);
            for (Object o : root) {
                JSONObject object = (JSONObject) o;
                if (!(object.get(INSERT) instanceof JSONObject)) {
                    result.append(StringEscapeUtils.unescapeJava(object.getString(INSERT)));
                }
            }
        } catch (Exception e){
            Document doc = Jsoup.parse(str);
            doc.body().children().forEach(element -> {
                String tagName = element.tag().getName();
                if(tagName == null){
                    result.append(element.text()).append("\n");
                    return;
                }
                switch (tagName){
                    case "figure":
                        break;
                    case "ol":
                    case "ul":
                        setListElementStr(result, element);
                        break;
                    default:
                        result.append(element.text()).append("\n");
                        break;
                }
            });
        }
        return result.toString().trim();
    }

    private String setListElementStr(StringBuilder result, Element element) {
        element.children().forEach(childElement -> result.append(getLiText(childElement)).append("\n"));
        return element.text();
    }

    private String getLiText(Element element) {
        StringBuilder result = new StringBuilder();
        String liAllText = element.text();
        StringBuilder childListText = new StringBuilder();
        StringBuilder childRelText = new StringBuilder();
        element.children().forEach(childElement -> {
            String tagName = childElement.tag().getName();
            if("ol".equals(tagName) || "ul".equals(tagName)){
                childListText.append(" ").append(setListElementStr(childRelText, childElement));
            }
        });
        if (childListText.length() > 0) {
            int childTextStart = liAllText.indexOf(childListText.toString());
            if (childTextStart > -1) {
                result.append(liAllText, 0, childTextStart);
            } else {
                result.append(liAllText);
            }
            result.append("\n").append(childRelText.toString());
        } else {
            result.append(liAllText);
        }
        return result.toString().trim();
    }

    protected String exportIssuesSprintName(ExportIssuesVO exportIssuesVO) {
        StringBuilder sprintName = new StringBuilder();
        if (!StringUtils.isEmpty(exportIssuesVO.getSprintName())){
            sprintName.append("正在使用冲刺:").append(exportIssuesVO.getSprintName());
        }
        if (!StringUtils.isEmpty(exportIssuesVO.getCloseSprintName())){
            if (sprintName.length() != 0){
                sprintName.append("\r\n");
            }
            sprintName.append("已关闭冲刺:").append(exportIssuesVO.getCloseSprintName());
        }
        return sprintName.toString();
    }

    protected String exportIssuesVersionName(ExportIssuesVO exportIssuesVO) {
        StringBuilder versionName = new StringBuilder();
        if (exportIssuesVO.getFixVersionName() != null && !"".equals(exportIssuesVO.getFixVersionName())) {
            versionName.append("修复的版本:").append(exportIssuesVO.getFixVersionName()).append("\r\n");
        } else if (exportIssuesVO.getInfluenceVersionName() != null && !"".equals(exportIssuesVO.getInfluenceVersionName())) {
            versionName.append("影响的版本:").append(exportIssuesVO.getInfluenceVersionName());
        }
        return versionName.toString();
    }

    protected String getOrderStrOfQueryingIssuesWithSub(Sort sort) {
        Map<String, String> order = new HashMap<>(1);
        order.put("issueId", "issue_id");
        return PageableHelper.getSortSql(PageUtil.sortResetOrder(sort, null, order));
    }
}
