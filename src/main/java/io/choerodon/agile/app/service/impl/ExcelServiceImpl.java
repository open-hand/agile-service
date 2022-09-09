package io.choerodon.agile.app.service.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monitorjbl.xlsx.StreamingReader;
import io.choerodon.agile.domain.entity.ExcelSheetData;
import io.choerodon.core.convertor.ApplicationContextHelper;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.hzero.excel.config.ExcelConfig;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;

import org.hzero.boot.file.FileClient;

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

    protected static final String IMPORT_TEMPLATE_NAME = "导入模板";
    protected static final String EPIC_CN = "史诗";
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

    

    private static final List<String> PROCESS_PREDEFINED_SYSTEM_FIELDS =
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
                    FieldCode.FEATURE,
                    FieldCode.EPIC,
                    FieldCode.LABEL,
                    FieldCode.ENVIRONMENT,
                    FieldCode.STATUS,
                    FieldCode.PRODUCT
            );

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
    protected RemoteIamOperator remoteIamOperator;

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
    private IssueLinkService issueLinkService;
    @Autowired
    private IssueLinkTypeMapper issueLinkTypeMapper;
    @Autowired
    private ObjectSchemeFieldExcelService objectSchemeFieldExcelService;
    @Autowired
    protected WorkLogMapper workLogMapper;
    @Autowired
    private LookupValueService lookupValueService;
    @Autowired
    protected IssueLinkMapper issueLinkMapper;
    @Autowired
    protected ExcelCommonService excelCommonService;

    private static final String[] FIELDS_NAMES;

    private static final String[] FIELDS;

    protected static Map<String, String> FIELD_MAP = new LinkedHashMap<>();

    protected static String[] AUTO_SIZE_WIDTH = {
            ExportIssuesVO.SUMMARY,
            ExportIssuesVO.EPIC_NAME,
            ExportIssuesVO.FEATURE,
            ExportIssuesVO.CREATION_DATE,
            ExportIssuesVO.LAST_UPDATE_DATE,
            ExportIssuesVO.SPRINT_NAME};

    static {
        FIELD_MAP.put(ExportIssuesVO.TYPE_NAME, IssueConstant.ISSUE_TYPE_CN);
        FIELD_MAP.put(ExportIssuesVO.ISSUE_NUM, IssueConstant.ISSUE_CN + "编号");
        FIELD_MAP.put(ExportIssuesVO.SUMMARY, "概要");
        FIELD_MAP.put(ExportIssuesVO.DESCRIPTION, "描述");
        FIELD_MAP.put(ExportIssuesVO.PRIORITY_NAME, "优先级");
        FIELD_MAP.put(ExportIssuesVO.STATUS_NAME, "状态");
        FIELD_MAP.put(ExportIssuesVO.RESOLUTION, "解决状态");
        FIELD_MAP.put(ExportIssuesVO.SPRINT_NAME, "冲刺");
        FIELD_MAP.put(ExportIssuesVO.ASSIGNEE_NAME, "经办人");
        FIELD_MAP.put(ExportIssuesVO.REPORTER_NAME, "报告人");
        FIELD_MAP.put(ExportIssuesVO.STORY_POINTS, "故事点");
        FIELD_MAP.put(ExportIssuesVO.REMAINING_TIME, "剩余预估时间");
        FIELD_MAP.put(ExportIssuesVO.ESTIMATE_TIME, "原始预估时间");
        FIELD_MAP.put(ExportIssuesVO.VERSION_NAME, "版本");
        FIELD_MAP.put(ExportIssuesVO.FIX_VERSION_NAME, "修复的版本");
        FIELD_MAP.put(ExportIssuesVO.INFLUENCE_VERSION_NAME, "影响的版本");
        FIELD_MAP.put(ExportIssuesVO.EPIC_NAME, "所属史诗");
        FIELD_MAP.put(ExportIssuesVO.LABEL_NAME, "标签");
        FIELD_MAP.put(ExportIssuesVO.COMPONENT_NAME, "模块");
        FIELD_MAP.put(ExportIssuesVO.CREATION_DATE, "创建时间");
        FIELD_MAP.put(ExportIssuesVO.LAST_UPDATE_DATE, "最后更新时间");
        FIELD_MAP.put(ExportIssuesVO.ESTIMATED_START_TIME, "预计开始时间");
        FIELD_MAP.put(ExportIssuesVO.ESTIMATED_END_TIME, "预计结束时间");
        FIELD_MAP.put(ExportIssuesVO.ACTUAL_START_TIME, "实际开始时间");
        FIELD_MAP.put(ExportIssuesVO.ACTUAL_END_TIME, "实际结束时间");
        FIELD_MAP.put(ExportIssuesVO.CREATED_USER_NAME, "创建人");
        FIELD_MAP.put(ExportIssuesVO.LAST_UPDATE_USER_NAME, "更新人");
        FIELD_MAP.put(ExportIssuesVO.MAIN_RESPONSIBLE_NAME, "主要负责人");
        FIELD_MAP.put(ExportIssuesVO.ENVIRONMENT_NAME, "环境");
        FIELD_MAP.put(ExportIssuesVO.SPENT_WORK_TIME, "已耗费时间");
        FIELD_MAP.put(ExportIssuesVO.ALL_ESTIMATE_TIME, "当前预估时间");
        FIELD_MAP.put(ExportIssuesVO.TAGS, "Tag");
        FIELD_MAP.put(ExportIssuesVO.RELATED_ISSUE, "关联" + IssueConstant.ISSUE_CN);
        FIELD_MAP.put(ExportIssuesVO.EPIC_SELF_NAME, "史诗名称");
        FIELD_MAP.put(ExportIssuesVO.PARTICIPANT, "参与人");
        FIELD_MAP.put(ExportIssuesVO.PRODUCT, "产品");
        FIELDS = new ArrayList<>(FIELD_MAP.keySet()).toArray(new String[FIELD_MAP.keySet().size()]);
        FIELDS_NAMES = new ArrayList<>(FIELD_MAP.values()).toArray(new String[FIELD_MAP.values().size()]);
    }

    private boolean withFeature(Long projectId, Long organizationId) {
        if (agilePluginService == null) {
            return false;
        }
        return !ObjectUtils.isEmpty(agilePluginService.getProgram(organizationId, projectId));
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
                processSystemFieldPredefinedList(projectId, systemFields, withFeature, cursor);
        Map<String, String> customFieldCodeNameMap = new HashMap<>();
        String issueTypeList = ProjectCategory.getProjectIssueTypeList(projectId);
        predefinedList.addAll(excelCommonService.processCustomFieldPredefinedList(projectId, customFields, cursor, systemFields.size(), customFieldCodeNameMap, issueTypeList));
        List<String> headers = generateExcelHeaderTitle(systemFields, customFields, customFieldCodeNameMap);
        Workbook wb = new XSSFWorkbook();
        // copy guide sheet
        excelCommonService.copyGuideSheetFromTemplate(wb, "/templates/IssueImportGuideTemplate.xlsx");
        Sheet sheet = wb.createSheet(IMPORT_TEMPLATE_NAME);
        CellStyle style = CatalogExcelUtil.getHeadStyle(wb);
        ExcelUtil.generateHeaders(sheet, style, headers);
        try {
            //填充预定义值
            excelCommonService.fillInPredefinedValues(wb, sheet, predefinedList);
            wb.write(response.getOutputStream());
        } catch (Exception e) {
            LOGGER.info("exception: {0}", e);
        }
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

    private List<PredefinedDTO> processSystemFieldPredefinedList(Long projectId,
                                                                 List<String> systemFields,
                                                                 boolean withFeature,
                                                                 ExcelImportTemplate.Cursor cursor) {
        List<PredefinedDTO> result = new ArrayList<>();
        result.add(processIssueTypePredefined(withFeature, projectId, cursor, systemFields));
        result.add(processParentIssuePredefined(projectId, cursor, systemFields));
        PROCESS_PREDEFINED_SYSTEM_FIELDS.forEach(fieldCode ->
                Optional.ofNullable(excelCommonService.processSystemFieldPredefined(projectId, cursor, withFeature, systemFields, fieldCode))
                        .ifPresent(result::add)
        );
        return result;
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
            if (!ExportIssuesVO.FEATURE.equals(typeCode)) {
                values.add(typeName);
            }
            if ("bug".equals(typeCode) && "system".equals(i.getSource())) {
                values.add(SUB_BUG_CN);
            }
        });
        int col = excelCommonService.getColByFieldCode(fieldCodes, FieldCode.ISSUE_TYPE);
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
        int col = excelCommonService.getColByFieldCode(systemFields, ExcelImportTemplate.IssueHeader.PARENT);
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
     * @param sheet sheet
     * @param columnNum 数据页总共有多少列数据
     * @return result
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

    @Async("issueImportExecutor")
    @Override
    public void batchImport(Long projectId,
                            Long organizationId,
                            Long userId,
                            InputStream inputStream,
                            ServletRequestAttributes requestAttributes) {
        RequestContextHolder.setRequestAttributes(requestAttributes);
        String websocketKey = WEBSOCKET_IMPORT_CODE + "-" + projectId;
        FileOperationHistoryDTO history = initFileOperationHistory(projectId, userId, DOING, UPLOAD_FILE, websocketKey);
        ExcelSheetData excelSheetData = readExcelSheetDataFromInputStream(inputStream, websocketKey, history);
        JSONObject sheetData = excelSheetData.getSheetData();
        if (ObjectUtils.isEmpty(sheetData.get("0"))) {
            //判断header行是否为空，为空抛异常
            history.setStatus("empty_data_sheet");
            updateHistoryAndSendMsg(history, websocketKey, "error.sheet.empty");
        }
        List<String> headerNames = excelSheetData.queryHeaderList();
        boolean withFeature = (agilePluginService != null && withFeature(projectId, organizationId));
        Map<Integer, ExcelColumnVO> headerMap = new LinkedHashMap<>();
        processHeaderMap(projectId, organizationId, headerNames, headerMap, withFeature, history, websocketKey);
        validateRequiredSystemField(headerMap, withFeature, history);
        Map<Integer, Set<Integer>> parentSonMap = new HashMap<>();
        Map<Integer, Integer> sonParentMap = new HashMap<>();
        Set<Integer> withoutParentRows = new HashSet<>();
        int issueTypeCol = getColIndexByFieldCode(headerMap, FieldCode.ISSUE_TYPE);
        int parentCol = getColIndexByFieldCode(headerMap, ExcelImportTemplate.IssueHeader.PARENT);
        processParentSonRelationship(parentSonMap, sonParentMap, withoutParentRows, excelSheetData, issueTypeCol, parentCol, headerMap);
        ExcelImportTemplate.Progress progress = new ExcelImportTemplate.Progress();
        List<Long> importedIssueIds = new ArrayList<>();
        List<RelatedIssueVO> relatedIssueList = new ArrayList<>();
        int lastSendCountNum = 0;
        Map<Long, List<String>> requireFieldMap = new HashMap<>();
        List<TriggerCarrierVO> triggerCarrierVOS = new ArrayList<>();
        Integer dataRowCount = excelSheetData.getRowNum();
        for (int currentRowNum = 1; currentRowNum <= dataRowCount; currentRowNum++) {
            if (Boolean.TRUE.equals(checkCanceled(projectId, history.getId(), importedIssueIds))) {
                return;
            }
            JSONObject rowJson = (JSONObject) sheetData.get(currentRowNum);
            if (ObjectUtils.isEmpty(rowJson)) {
                continue;
            }
            JSONObject issueTypeJson = (JSONObject) rowJson.get(issueTypeCol);
            String issueType;
            if (ObjectUtils.isEmpty(issueTypeJson)
                    || ObjectUtils.isEmpty(issueTypeJson.getString(ExcelSheetData.STRING_CELL))) {
                if (ObjectUtils.isEmpty(issueTypeJson)) {
                    issueTypeJson = new JSONObject();
                }
                String errorMsg = buildWithErrorMsg("", IssueConstant.ISSUE_TYPE_CN + "为空");
                excelCommonService.putErrorMsg(rowJson, issueTypeJson, errorMsg);
                lastSendCountNum = putErrorMsgAndSendMsg(userId, websocketKey, history, progress, lastSendCountNum, dataRowCount);
                continue;
            }

            issueType = issueTypeJson.getString(ExcelSheetData.STRING_CELL);
            String issueTypeCode = getIssueTypeCode(headerMap, issueType);
            Set<Integer> sonSet = parentSonMap.get(currentRowNum);
            boolean hasSonNodes = !ObjectUtils.isEmpty(sonSet);
            if ((IssueTypeCode.isStory(issueTypeCode)
                    || IssueTypeCode.isTask(issueTypeCode)
                    || IssueTypeCode.isBug(issueTypeCode))
                    && hasSonNodes) {
                List<Long> insertIds = new ArrayList<>();
                try {
                    IssueCreateVO parent = new IssueCreateVO();
                    validateData(projectId, rowJson, headerMap, withoutParentRows, parent, null, issueTypeCol, parentCol, requireFieldMap);
                    if (Boolean.TRUE.equals(rowJson.getBoolean(ExcelSheetData.JSON_KEY_IS_ERROR))) {
                        lastSendCountNum = excelCommonService.processErrorData(userId, history, sheetData, dataRowCount, progress, currentRowNum, sonSet, parentCol, lastSendCountNum, websocketKey);
                        currentRowNum = Collections.max(sonSet);
                        continue;
                    }
                    List<ComponentIssueRelVO> components = parent.getComponentIssueRelVOList();
                    Long sprintId = parent.getSprintId();
                    Long epicId = parent.getEpicId();
                    Optional.ofNullable(parent.getRelatedIssueVO()).ifPresent(relatedIssueList::add);
                    IssueVO result = stateMachineClientService.createIssueWithoutRuleNotice(parent, APPLY_TYPE_AGILE);
                    insertIds.add(result.getIssueId());
                    excelCommonService.insertCustomFields(result.getIssueId(), parent.getCustomFields(), projectId);
                    List<Long> customFieldIds = new ArrayList<>();
                    if (!CollectionUtils.isEmpty(parent.getCustomFields())) {
                        customFieldIds.addAll(parent.getCustomFields().stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList()));
                    }
                    issueService.buildTriggerCarrierVO(projectId, result.getIssueId(), triggerCarrierVOS, customFieldIds);
                    rowJson.put(ExcelSheetData.JSON_KEY_ISSUE_ID, result.getIssueId());

                    result.setComponentIssueRelVOList(components);
                    result.setSprintId(sprintId);
                    result.setEpicId(epicId);

                    boolean sonsOk = true;
                    Map<Integer, IssueCreateVO> sonMap = new HashMap<>();
                    for (Integer sonRowNum : sonSet) {
                        IssueCreateVO son = new IssueCreateVO();
                        JSONObject sonRowJson = (JSONObject) sheetData.get(sonRowNum);
                        validateData(projectId, sonRowJson, headerMap, withoutParentRows, son, result, issueTypeCol, parentCol, requireFieldMap);
                        if (Boolean.TRUE.equals(sonRowJson.getBoolean(ExcelSheetData.JSON_KEY_IS_ERROR))) {
                            //子节点有错误
                            sonsOk = false;
                            break;
                        } else {
                            sonMap.put(sonRowNum, son);
                        }
                    }
                    if (!sonsOk) {
                        lastSendCountNum = excelCommonService.processErrorData(userId, history, sheetData, dataRowCount, progress, currentRowNum, sonSet, parentCol, lastSendCountNum, websocketKey);
                        currentRowNum = Collections.max(sonSet);
                        issueService.batchDeleteIssuesAgile(projectId, insertIds);
                        continue;
                    }
                    sonMap.forEach((k, v) -> {
                        Optional.ofNullable(v.getRelatedIssueVO()).ifPresent(relatedIssueList::add);
                        IssueVO returnValue = stateMachineClientService.createIssueWithoutRuleNotice(v, APPLY_TYPE_AGILE);
                        insertIds.add(returnValue.getIssueId());
                        excelCommonService.insertCustomFields(returnValue.getIssueId(), v.getCustomFields(), projectId);
                        List<Long> subIssueCustomFieldIds = new ArrayList<>();
                        if (!CollectionUtils.isEmpty(v.getCustomFields())) {
                            subIssueCustomFieldIds.addAll(v.getCustomFields().stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList()));
                        }
                        issueService.buildTriggerCarrierVO(projectId, returnValue.getIssueId(), triggerCarrierVOS, subIssueCustomFieldIds);
                        JSONObject currentRowJson = (JSONObject) sheetData.get(k);
                        currentRowJson.put(ExcelSheetData.JSON_KEY_ISSUE_ID, returnValue.getIssueId());
                    });

                    importedIssueIds.add(result.getIssueId());
//                    importedIssueIds.addAll(rowIssueIdMap.values());
                    progress.addSuccessCount(sonSet.size() + 1L);
                    progress.addProcessNum(sonSet.size() + 1);
                    currentRowNum = Collections.max(sonSet);
                } catch (Exception e) {
                    LOGGER.error("insert data error when import excel, exception: {0}", e);
                    lastSendCountNum = excelCommonService.processErrorData(userId, history, sheetData, dataRowCount, progress, currentRowNum, sonSet, parentCol, lastSendCountNum, websocketKey);
                    currentRowNum = Collections.max(sonSet);
                    issueService.batchDeleteIssuesAgile(projectId, insertIds);
                    continue;
                }
            } else {
                IssueCreateVO issueCreateVO = new IssueCreateVO();
                validateData(projectId, rowJson, headerMap, withoutParentRows, issueCreateVO, null, issueTypeCol, parentCol, requireFieldMap);
                if (Boolean.TRUE.equals(rowJson.getBoolean(ExcelSheetData.JSON_KEY_IS_ERROR))) {
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
                excelCommonService.insertCustomFields(result.getIssueId(), issueCreateVO.getCustomFields(), projectId);
                List<Long> customFieldIds = new ArrayList<>();
                if (!CollectionUtils.isEmpty(issueCreateVO.getCustomFields())) {
                    customFieldIds.addAll(issueCreateVO.getCustomFields().stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList()));
                }
                issueService.buildTriggerCarrierVO(projectId, result.getIssueId(), triggerCarrierVOS, customFieldIds);
                rowJson.put(ExcelSheetData.JSON_KEY_ISSUE_ID, result.getIssueId());

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
        updateRelatedIssue(relatedIssueList, headerMap, sheetData, projectId, progress, parentSonMap, parentCol);
        issueService.batchCreateIssueInvokeTrigger(triggerCarrierVOS);
        //错误数据生成excel
        String status = excelCommonService.generateErrorDataExcelAndUpload(excelSheetData, headerMap, headerNames, history, organizationId, "/templates/IssueImportGuideTemplate.xlsx");
        excelCommonService.updateFinalRecode(history, progress.getSuccessCount(), progress.getFailCount(), status, websocketKey);
    }

    private ExcelSheetData readExcelSheetDataFromInputStream(InputStream inputStream,
                                                             String websocketKey,
                                                             FileOperationHistoryDTO history) {
        ExcelConfig excelConfig = ApplicationContextHelper.getContext().getBean(ExcelConfig.class);
        Workbook workbook = StreamingReader.builder().rowCacheSize(excelConfig.getCacheSize()).bufferSize(excelConfig.getBufferSize()).open(inputStream);
        excelCommonService.validateWorkbook(workbook, history, websocketKey);
        //excel转json
        int sheetNo = 1;
        Sheet sheet = workbook.getSheetAt(sheetNo);
        return readDate(sheet);
    }

//    private void getIssueTypeFromRowJson() {
//        if (ObjectUtils.isEmpty(issueTypeJson)) {
//            issueTypeJson = new JSONObject();
//            String errorMsg = buildWithErrorMsg("", IssueConstant.ISSUE_TYPE_CN + "为空");
//            lastSendCountNum = excelCommonService.putErrorMsgAndSendMsg(userId, websocketKey, history, progress, lastSendCountNum, dataRowCount, rowJson, issueTypeJson, errorMsg);
//        } else {
//            issueType = issueTypeJson.getString(ExcelSheetData.STRING_CELL);
//            if (StringUtils.isEmpty(issueType)) {
//                String errorMsg = buildWithErrorMsg("", IssueConstant.ISSUE_TYPE_CN + "为空");
//                lastSendCountNum = excelCommonService.putErrorMsgAndSendMsg(userId, websocketKey, history, progress, lastSendCountNum, dataRowCount, rowJson, issueTypeJson, errorMsg);
//            }
//        }
//
//    }

    private int putErrorMsgAndSendMsg(Long userId,
                                      String websocketKey,
                                      FileOperationHistoryDTO history,
                                      ExcelImportTemplate.Progress progress,
                                      int lastSendCountNum,
                                      Integer dataRowCount) {
        progress.failCountIncrease();
        progress.processNumIncrease();
        history.setFailCount(progress.getFailCount());
        if ((progress.getProcessNum() - lastSendCountNum) * 1.0 / dataRowCount >= 0.1) {
            lastSendCountNum = progress.getProcessNum();
            sendProcess(history, userId, progress.getProcessNum() * 1.0 / dataRowCount, websocketKey);
        }
        return lastSendCountNum;
    }

    

    private ExcelSheetData readDate(Sheet sheet) {
        JSONObject sheetData = new JSONObject();
        Iterator<Row> sheetIterator = sheet.rowIterator();
        int rowNum = 0;
        int colNum = 0;
        while(sheetIterator.hasNext()) {
            Row row = sheetIterator.next();
            if (rowNum == 0) {
                //header行
                for (int i=0;; i++) {
                    Cell cell = row.getCell(i);
                    if (isCellEmpty(cell)) {
                        break;
                    }
                    colNum = i;
                    putValueToSheetData(sheetData, rowNum, i, cell);
                }
                if (ObjectUtils.isEmpty(sheetData.get(String.valueOf(rowNum)))) {
                    //header行为空，跳出
                    break;
                }
            } else {
                for (int i=0; i<=colNum ; i++) {
                    Cell cell = row.getCell(i);
                    if (isCellEmpty(cell)) {
                        continue;
                    }
                    putValueToSheetData(sheetData, rowNum, i, cell);
                }
            }
            rowNum++;
        }
        return new ExcelSheetData(sheetData.size(), colNum, sheetData);
    }

    private void putValueToSheetData(JSONObject sheetData,
                                     int rowNum,
                                     int columnNum,
                                     Cell cell) {
        JSONObject rowJson = (JSONObject) sheetData.get(rowNum);
        if (ObjectUtils.isEmpty(rowJson)) {
            rowJson = new JSONObject();
            sheetData.put(String.valueOf(rowNum), rowJson);
        }
        JSONObject cellJson = new JSONObject();
        if (cell.getCellTypeEnum().equals(CellType.NUMERIC)
                && DateUtil.isCellDateFormatted(cell)) {
            //日期格式单元格
            cellJson.put(ExcelSheetData.DATE_CELL, cell.getDateCellValue());
        } else {
            cellJson.put(ExcelSheetData.STRING_CELL, cell.getStringCellValue().trim());
        }
        rowJson.put(String.valueOf(columnNum), cellJson);
        rowJson.put(ExcelSheetData.JSON_KEY_ROW_NUM, rowNum);
        sheetData.put(String.valueOf(rowNum), rowJson);
    }

    private void updateHistoryAndSendMsg(FileOperationHistoryDTO history,
                                         String websocketKey,
                                         String exceptionMsg) {
        fileOperationHistoryMapper.updateByPrimaryKeySelective(history);
        FileOperationHistoryDTO errorImport = fileOperationHistoryMapper.selectByPrimaryKey(history.getId());
        sendProcess(errorImport, history.getUserId(), 0.0, websocketKey);
        if (!ObjectUtils.isEmpty(exceptionMsg)) {
            throw new CommonException("error.sheet.empty");
        }
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
                                    Map<Integer, ExcelColumnVO> headerMap,
                                    JSONObject sheetData,
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
            JSONObject rowJson = (JSONObject)sheetData.get(rowNum);
            JSONObject cellJson = (JSONObject)rowJson.get(relateIssueIndex);
            String value = cellJson.getString(ExcelSheetData.STRING_CELL);
            Long issueId = rowJson.getLong(ExcelSheetData.JSON_KEY_ISSUE_ID);
            boolean currentRowIsError = Boolean.TRUE.equals(rowJson.getBoolean(ExcelSheetData.JSON_KEY_IS_ERROR));
            if (issueId != null && !currentRowIsError) {
                Set<Integer> relatedRows = x.getRelatedRows();
                Set<Long> relatedIssueIds = x.getRelatedIds();
                boolean ok = true;
                for (Integer relatedRow : relatedRows) {
                    if (Objects.equals(rowNum, relatedRow)) {
                        deleteIssueIds.add(issueId);
                        String errorMsg = buildWithErrorMsg(value, "自己不能和自己关联，rowNum: " + (rowNum + 1));
                        excelCommonService.putErrorMsg(rowJson, cellJson, errorMsg);
                        ok = false;
                        progress.failCountIncrease();
                        progress.successCountDecrease();
                        Set<Integer> sonSet = parentSonMap.get(rowNum);
                        if (!CollectionUtils.isEmpty(sonSet)) {
                            sonSet.forEach(v -> {
                                Long sonIssueId = ((JSONObject)sheetData.get(v)).getLong(ExcelSheetData.JSON_KEY_ISSUE_ID);
                                if (!ObjectUtils.isEmpty(sonIssueId)) {
                                    deleteIssueIds.add(sonIssueId);
                                    progress.failCountIncrease();
                                    progress.successCountDecrease();
                                }
                            });
                            excelCommonService.setErrorMsgToParentSonRow(rowNum, sheetData, sonSet, parentCol);
                        }
                        break;
                    }

                    JSONObject relatedRowJson = (JSONObject)sheetData.get(relatedRow);
                    if (!ObjectUtils.isEmpty(relatedRowJson)
                            && !ObjectUtils.isEmpty(relatedRowJson.getLong(ExcelSheetData.JSON_KEY_ISSUE_ID))) {
                        deleteIssueIds.add(issueId);
                        String errorMsg = buildWithErrorMsg(value, "第" + (relatedRow + 1) + "行" + IssueConstant.ISSUE_CN + "不存在");
                        excelCommonService.putErrorMsg(rowJson, cellJson, errorMsg);
                        ok = false;
                        progress.failCountIncrease();
                        progress.successCountDecrease();
                        Set<Integer> sonSet = parentSonMap.get(rowNum);
                        if (!CollectionUtils.isEmpty(sonSet)) {
                            sonSet.forEach(v -> {
                                JSONObject sonRowJson = (JSONObject)sheetData.get(v);
                                if (!ObjectUtils.isEmpty(sonRowJson)
                                        && !ObjectUtils.isEmpty(sonRowJson.getLong(ExcelSheetData.JSON_KEY_ISSUE_ID))) {
                                    Long sonIssueId = sonRowJson.getLong(ExcelSheetData.JSON_KEY_ISSUE_ID);
                                    deleteIssueIds.add(sonIssueId);
                                    progress.failCountIncrease();
                                    progress.successCountDecrease();
                                }
                            });
                            excelCommonService.setErrorMsgToParentSonRow(rowNum, sheetData, sonSet, parentCol);
                        }
                        break;
                    } else {
                        relatedIssueIds.add(relatedRowJson.getLong(ExcelSheetData.JSON_KEY_ISSUE_ID));
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

    private void validateData(Long projectId,
                              JSONObject rowJson,
                              Map<Integer, ExcelColumnVO> headerMap,
                              Set<Integer> withoutParentRows,
                              IssueCreateVO issueCreateVO,
                              IssueVO parentIssue,
                              int issueTypeCol,
                              int parentCol,
                              Map<Long, List<String>> requireFieldMap) {
        int rowNum = rowJson.getInteger(ExcelSheetData.JSON_KEY_ROW_NUM);
        issueCreateVO.setProjectId(projectId);
        JSONObject issueTypeJson = (JSONObject) rowJson.get(issueTypeCol);
        String value = "";
        if (ObjectUtils.isEmpty(issueTypeJson)
                || ObjectUtils.isEmpty(issueTypeJson.getString(ExcelSheetData.STRING_CELL))) {
            if (ObjectUtils.isEmpty(issueTypeJson)) {
                issueTypeJson = new JSONObject();
            }
            String errorMsg = buildWithErrorMsg(value, IssueConstant.ISSUE_TYPE_CN + "为空");
            excelCommonService.putErrorMsg(rowJson, issueTypeJson, errorMsg);
            return;
        }
        value = issueTypeJson.getString(ExcelSheetData.STRING_CELL);
        if (withoutParentRows.contains(rowNum)) {
            String errorMsg = buildWithErrorMsg(value, "子任务/子缺陷必须要有父节点");
            excelCommonService.putErrorMsg(rowJson, issueTypeJson, errorMsg);
            return;
        }
        String issueTypeCode = getIssueTypeCode(headerMap, value);
        if (parentIssue == null
                && (IssueTypeCode.isSubTask(issueTypeCode)
                || SUB_BUG_CN.equals(value))) {
            JSONObject parentJson = (JSONObject)rowJson.get(parentCol);
            String parentCellValue = "";
            if (ObjectUtils.isEmpty(parentJson)
                    || ObjectUtils.isEmpty(parentJson.getString(ExcelSheetData.STRING_CELL))) {
                if (ObjectUtils.isEmpty(parentJson)) {
                    parentJson = new JSONObject();
                }
                String errorMsg = buildWithErrorMsg(parentCellValue, "子任务/子缺陷必须要有父节点");
                excelCommonService.putErrorMsg(rowJson, parentJson, errorMsg);
                return;
            }
            parentCellValue = parentJson.getString(ExcelSheetData.STRING_CELL);
            List<String> values = headerMap.get(parentCol).getPredefinedValues();
            String issueNum = parentCellValue.split(COLON_CN)[0];
            if (!values.contains(issueNum)) {
                String errorMsg = buildWithErrorMsg(parentCellValue, "输入的父级编号有误");
                excelCommonService.putErrorMsg(rowJson, parentJson, errorMsg);
                return;
            }
            parentIssue = issueMapper.selectByIssueNum(projectId, issueNum);
            if (parentIssue == null) {
                String errorMsg = buildWithErrorMsg(parentCellValue, "父节点不存在");
                excelCommonService.putErrorMsg(rowJson, issueTypeJson, errorMsg);
                return;
            }
            IssueDTO issueDTO = issueMapper.queryIssueSprintNotClosed(projectId, parentIssue.getIssueId());
            parentIssue.setSprintId(issueDTO.getSprintId());
        }
        for (Map.Entry<Integer, ExcelColumnVO> entry : headerMap.entrySet()) {
            Integer col = entry.getKey();
            ExcelColumnVO excelColumn = entry.getValue();
            boolean isCustomField = excelColumn.isCustomField();
            if (isCustomField) {
                excelCommonService.validateCustomFieldData(rowJson, col, excelColumn, issueCreateVO);
            } else {
                excelCommonService.validateCommonSystemFieldData(rowJson, col, excelColumn, issueCreateVO, parentIssue, projectId, headerMap);
            }
            excelCommonService.handlerRequireFiled(excelColumn, requireFieldMap, issueCreateVO, projectId);
            Boolean checkRequireField = excelCommonService.checkRequireField(requireFieldMap, excelColumn, issueCreateVO, rowJson, col);
            if (!checkRequireField) {
                break;
            }
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

    protected Integer getColIndexByFieldCode(Map<Integer, ExcelColumnVO> headerMap, String fieldCode) {
        for (Map.Entry<Integer, ExcelColumnVO> entry : headerMap.entrySet()) {
            if (entry.getValue().getFieldCode().equals(fieldCode)) {
                return entry.getKey();
            }
        }
        return null;
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
                                              ExcelSheetData excelSheetData,
                                              int issueTypeCol,
                                              int parentCol,
                                              Map<Integer, ExcelColumnVO> headerMap) {
        Map<Integer, String> rowIssueTypeMap = new LinkedHashMap<>();
        List<IssueTypeLinkDTO> issueTypeLinks = new ArrayList<>();
        Integer rowNum = excelSheetData.getRowNum();
        JSONObject dataSheet = excelSheetData.getSheetData();
        for (int i = 1; i <= rowNum; i++) {
            int size = issueTypeLinks.size();
            IssueTypeLinkDTO lastIssueTypeLink = null;
            if (size > 0) {
                lastIssueTypeLink = issueTypeLinks.get(size - 1);
            }
            JSONObject rowJson = (JSONObject)dataSheet.get(String.valueOf(i));
            //为空则表示为空行
            if (ObjectUtils.isEmpty(rowJson)) {
                continue;
            }
            JSONObject cellJson = (JSONObject)rowJson.get(issueTypeCol);
            if (ObjectUtils.isEmpty(cellJson)) {
                continue;
            }
            String issueType = cellJson.getString(ExcelSheetData.STRING_CELL);
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
            Integer currentRowNum = entry.getKey();
            String issueType = entry.getValue();
            String issueTypeCode = getIssueTypeCode(headerMap, issueType);
            if (IssueTypeCode.isSubTask(issueTypeCode)
                    || SUB_BUG_CN.equals(issueType)) {
                Integer parentRow = sonParentMap.get(currentRowNum);
                if (parentRow == null) {
                    JSONObject rowJson = (JSONObject)dataSheet.get(String.valueOf(currentRowNum));
                    JSONObject cellJson = (JSONObject)rowJson.get(parentCol);
                    if (ObjectUtils.isEmpty(cellJson)) {
                        withoutParentRows.add(currentRowNum);
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
                                  String websocketKey) {
        boolean containsCustomFields = false;
        for (int i = 0; i < headerNames.size(); i++) {
            String headerName = headerNames.get(i);
            String fieldCode = ExcelImportTemplate.IssueHeader.getCodeByValue(headerName);
            boolean isSystemField = !StringUtils.isEmpty(fieldCode);
            ExcelColumnVO excelColumnVO = new ExcelColumnVO();
            headerMap.put(i, excelColumnVO);
            excelColumnVO.setCustomField(!isSystemField);
            if (isSystemField) {
                excelCommonService.addSystemFieldIfDateType(fieldCode, i, excelColumnVO);
                excelColumnVO.setFieldCode(fieldCode);
                setSystemFieldPredefinedValueByCode(fieldCode, projectId, organizationId, excelColumnVO, withFeature);
            } else {
                containsCustomFields = true;
                excelColumnVO.setFieldCode(headerName);
            }
        }
        if (containsCustomFields) {
            String issueTypeList = ProjectCategory.getProjectIssueTypeList(projectId);
            excelCommonService.validateCustomField(headerMap, projectId, history, issueTypeList, null, websocketKey);
        }
    }

    private void setSystemFieldPredefinedValueByCode(String code,
                                                     Long projectId,
                                                     Long organizationId,
                                                     ExcelColumnVO excelColumnVO,
                                                     boolean withFeature) {
        switch (code) {
            case FieldCode.ISSUE_TYPE:
                processIssueType(withFeature, projectId, excelColumnVO);
                break;
            case ExcelImportTemplate.IssueHeader.PARENT:
                processParentIssue(projectId, excelColumnVO);
                break;
            case FieldCode.ISSUE_STATUS:
                processIssueStatus(projectId, excelColumnVO);
                break;
            default:
                excelCommonService.setCommonSystemFieldPredefinedValueByCode(code, projectId, organizationId, excelColumnVO, withFeature);
                break;
        }
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
            if (!ExportIssuesVO.FEATURE.equals(typeCode)) {
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
                double process = excelCommonService.getProcess(cursor.getPage(), page.getTotalPages());
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
        exportIssuesVO.setDescription(RichTextUtil.getDes(exportIssuesVO.getDescription()));
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
        if (!ObjectUtils.isEmpty(issueLinkList)) {
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
     * @param workLogVOMap workLogVOMap
     * @param exportIssuesVO exportIssuesVO
     */
    private void setSpentWorkTimeAndAllEstimateTime(Map<Long, List<WorkLogVO>> workLogVOMap, ExportIssuesVO exportIssuesVO) {
        List<WorkLogVO> workLogVOList = workLogVOMap.get(exportIssuesVO.getIssueId());
        BigDecimal spentWorkTime = null;
        BigDecimal allEstimateTime;
        if (!CollectionUtils.isEmpty(workLogVOList)) {
            spentWorkTime = new BigDecimal(0);
            for (WorkLogVO workLogVO : workLogVOList) {
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

    /**
     * 上传文件到minio中
     *
     * @param organizationId organizationId
     * @param workbook workbook
     * @param fileName fileName
     * @param fileOperationHistoryDTO fileOperationHistoryDTO
     * @param userId userId
     */
    @Override
    public void downloadWorkBook(Long organizationId, Workbook workbook, String fileName, FileOperationHistoryDTO fileOperationHistoryDTO, Long userId) {
        try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
            workbook.write(os);
            byte[] content = os.toByteArray();
            MultipartFile file = new MultipartExcel("file", fileName, EXCELCONTENTTYPE, content);

            //返回上载结果
            String path = fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), null, fileName, file);
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
        List<IssueSprintDTO> issueSprintList = issue.getIssueSprintDTOS();
        if (!ObjectUtils.isEmpty(issueSprintList)) {
            for (IssueSprintDTO sprint : issueSprintList) {
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
        result.add(ExportIssuesVO.TYPE_NAME);
        result.add(ExportIssuesVO.SUMMARY);
        result.add(ExportIssuesVO.ISSUE_NUM);
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
     * @param exportFieldCodes exportFieldCodes
     * @return result
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

    protected String exportIssuesSprintName(ExportIssuesVO exportIssuesVO) {
        StringBuilder sprintName = new StringBuilder();
        if (!StringUtils.isEmpty(exportIssuesVO.getSprintName())) {
            sprintName.append("正在使用冲刺:").append(exportIssuesVO.getSprintName());
        }
        if (!StringUtils.isEmpty(exportIssuesVO.getCloseSprintName())) {
            if (sprintName.length() != 0) {
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
