package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.infra.enums.ExcelImportTemplateColumn;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.IssueTypeLinkDTO;
import io.choerodon.agile.infra.dto.PredefinedDTO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.hzero.boot.file.FileClient;
import org.hzero.boot.message.MessageClient;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class ExcelServiceImpl implements ExcelService {

    protected static final Logger LOGGER = LoggerFactory.getLogger(ExcelServiceImpl.class);

    protected static final String[] FIELDS_NAME =
            {"问题类型*", "所属史诗", "模块", "冲刺", "概述*", "子任务概述(仅子任务生效)", "描述", "经办人", "报告人",
                    "优先级*", "预估时间(小时)", "版本", "故事点", "史诗名称(仅问题类型为史诗时生效)"};

    protected static final String BACKETNAME = "agile-service";
    protected static final String SUB_TASK = "sub_task";
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
    protected static final String DOWNLOAD_FILE_PI = "download_file_pi";
    protected static final String DOWNLOAD_FILE_ISSUE_ANALYSIS = "download_file_issue_analysis";
    private static final String EXPORT_ERROR_WORKBOOK_CLOSE = "error.issue.close.workbook";
    private static final String PROJECT_ERROR = "error.project.notFound";
    private static final String FIX_RELATION_TYPE = "fix";
    private static final String INFLUENCE_RELATION_TYPE = "influence";
    private Log log = LogFactory.getLog(this.getClass());

    protected static final String VERSION_PLANNING = "version_planning";

    protected static final String RELATION_TYPE_FIX = "fix";
    protected static final String IMPORT_TEMPLATE_NAME = "导入模板";

    protected static final String EPIC_CN = "史诗";

    protected static final String STORY_CN = "故事";

    protected static final String BUG_CN = "缺陷";

    protected static final String TASK_CN = "任务";

    protected static final String SUB_TASK_CN = "子任务";

    @Autowired
    protected StateMachineClientService stateMachineClientService;

    @Autowired
    private MessageClient messageClient;

    @Autowired
    private FileOperationHistoryMapper fileOperationHistoryMapper;

    @Autowired
    protected ProductVersionMapper productVersionMapper;

    @Autowired
    private IssueService issueService;

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

    private static final String[] FIELDS_NAMES;

    private static final String[] FIELDS;

    protected static Map<String, String> FIELD_MAP = new LinkedHashMap<>();

    protected static String[] AUTO_SIZE_WIDTH = {"summary", "epicName", "feature",
            "creationDate", "lastUpdateDate", "sprintName"};

    static {
        FIELD_MAP.put("typeName", "问题类型");
        FIELD_MAP.put("issueNum", "问题编号");
        FIELD_MAP.put("summary", "概要");
        FIELD_MAP.put("description", "描述");
        FIELD_MAP.put("priorityName", "优先级");
        FIELD_MAP.put("statusName", "状态");
        FIELD_MAP.put("resolution", "解决状态");
        FIELD_MAP.put("sprintName", "冲刺");
        FIELD_MAP.put("assigneeName", "经办人");
        FIELD_MAP.put("reporterName", "报告人");
        FIELD_MAP.put("storyPoints", "故事点");
        FIELD_MAP.put("remainingTime", "剩余预估时间");
        FIELD_MAP.put("versionName", "版本");
        FIELD_MAP.put("epicName", "所属史诗");
        FIELD_MAP.put("labelName", "标签");
        FIELD_MAP.put("componentName", "模块");
        FIELD_MAP.put("creationDate", "创建时间");
        FIELD_MAP.put("lastUpdateDate", "最后更新时间");
        FIELD_MAP.put("estimatedStartTime", "预计开始时间");
        FIELD_MAP.put("estimatedEndTime", "预计结束时间");
        FIELDS = new ArrayList<>(FIELD_MAP.keySet()).toArray(new String[FIELD_MAP.keySet().size()]);
        FIELDS_NAMES = new ArrayList<>(FIELD_MAP.values()).toArray(new String[FIELD_MAP.values().size()]);
    }

    @Override
    public void download(Long projectId, Long organizationId, HttpServletRequest request, HttpServletResponse response) {
        List<PredefinedDTO> predefinedList = getPredefinedList(organizationId, projectId, false);
        //所属史诗预定义值
        predefinedList.add(getEpicPredefined(projectId));

        Workbook wb = new XSSFWorkbook();
        // create guide sheet
        ExcelUtil.createGuideSheet(wb, ExcelUtil.initGuideSheet(), false);
        Sheet sheet = wb.createSheet(IMPORT_TEMPLATE_NAME);
        CellStyle style = CatalogExcelUtil.getHeadStyle(wb);
        Map<Integer,Integer> widthMap = new HashMap<>();
        widthMap.put(ExcelImportTemplateColumn.Issue.EPIC_COL, 8000);
        widthMap.put(ExcelImportTemplateColumn.Issue.SUB_TASK_COL, 8000);
        widthMap.put(ExcelImportTemplateColumn.Issue.EPIC_NAME_COL, 8000);
        ExcelUtil.generateHeaders(sheet, style, Arrays.asList(FIELDS_NAME), widthMap);

        try {
            //填充预定义值
            fillInPredefinedValues(wb, sheet, predefinedList);
            wb.write(response.getOutputStream());
        } catch (Exception e) {
            LOGGER.info("exception: {}", e);
        }
    }

    protected PredefinedDTO getEpicPredefined(Long projectId) {
        List<String> values = new ArrayList<>(getEpicMap(projectId).keySet());
        values.sort(String.CASE_INSENSITIVE_ORDER);
        return new PredefinedDTO(
                values,
                1,
                500,
                ExcelImportTemplateColumn.Issue.EPIC_SHEET.getCol(),
                ExcelImportTemplateColumn.Issue.EPIC_SHEET.getCol(),
                ExcelImportTemplateColumn.Issue.EPIC_SHEET.getName(),
                ExcelImportTemplateColumn.Issue.EPIC_SHEET.getIndex()
        );
    }

    protected Map<String, Long> getEpicMap(Long projectId) {
        Map<String, Long> epicMap = new HashMap<>();
        List<EpicDataVO> epics = issueService.listEpic(projectId);
        epics.forEach(e -> {
            String summary = e.getSummary();
            if (ObjectUtils.isEmpty(epicMap.get(summary))) {
                epicMap.put(summary, e.getIssueId());
            }
        });
        return epicMap;
    }

    protected void fillInPredefinedValues(Workbook wb, Sheet sheet, List<PredefinedDTO> predefinedList) {
        for (PredefinedDTO predefined : predefinedList) {
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

    protected List<PredefinedDTO> getPredefinedList(Long organizationId, Long projectId, boolean withFeature) {
        List<PredefinedDTO> predefinedList = new ArrayList<>();
        List<PriorityVO> priorityVOList = priorityService.queryByOrganizationIdList(organizationId);
        List<IssueTypeVO> issueTypeVOList = projectConfigService.queryIssueTypesByProjectId(projectId, APPLY_TYPE_AGILE);
        List<ProductVersionCommonDTO> productVersionCommonDTOList = productVersionMapper.listByProjectId(projectId);
        List<IssueComponentDTO> issueComponentDTOList = issueComponentMapper.selectByProjectId(projectId);
        List<SprintDTO> sprintDTOList = sprintMapper.selectNotDoneByProjectId(projectId);

        List<String> priorityList = new ArrayList<>();
        for (PriorityVO priorityVO : priorityVOList) {
            if (priorityVO.getEnable()) {
                priorityList.add(priorityVO.getName());
            }
        }
        predefinedList.add(
                new PredefinedDTO(
                        priorityList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.PRIORITY_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.PRIORITY_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.PRIORITY_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.PRIORITY_SHEET.getIndex()
                ));

        List<String> issueTypeList = new ArrayList<>();
        for (IssueTypeVO issueTypeVO : issueTypeVOList) {
            String typeCode = issueTypeVO.getTypeCode();
            if (withFeature && "issue_epic".equals(typeCode)) {
                continue;
            }
            if (!SUB_TASK.equals(typeCode) && !FEATURE.equals(typeCode)) {
                issueTypeList.add(issueTypeVO.getName());
            }
        }
        predefinedList.add(
                new PredefinedDTO(
                        issueTypeList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.ISSUE_TYPE_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.ISSUE_TYPE_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.ISSUE_TYPE_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.ISSUE_TYPE_SHEET.getIndex()
                ));

        List<String> versionList = new ArrayList<>();
        for (ProductVersionCommonDTO productVersionCommonDTO : productVersionCommonDTOList) {
            if (VERSION_PLANNING.equals(productVersionCommonDTO.getStatusCode())) {
                versionList.add(productVersionCommonDTO.getName());
            }
        }
        predefinedList.add(
                new PredefinedDTO(
                        versionList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.FIX_VERSION_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.FIX_VERSION_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.FIX_VERSION_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.FIX_VERSION_SHEET.getIndex()
                ));

        List<String> componentList = new ArrayList<>();
        for (IssueComponentDTO issueComponentDTO : issueComponentDTOList) {
            componentList.add(issueComponentDTO.getName());
        }
        predefinedList.add(
                new PredefinedDTO(
                        componentList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.COMPONENT_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.COMPONENT_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.COMPONENT_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.COMPONENT_SHEET.getIndex()
                ));

        List<String> sprintList = new ArrayList<>();
        for (SprintDTO sprintDTO : sprintDTOList) {
            sprintList.add(sprintDTO.getSprintName());
        }
        predefinedList.add(
                new PredefinedDTO(
                        sprintList,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.SPRINT_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.SPRINT_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.SPRINT_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.SPRINT_SHEET.getIndex()
                ));

        List<String> users = new ArrayList<>(getManagers(projectId).keySet());
        predefinedList.add(
                new PredefinedDTO(
                        users,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.MANAGER_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.MANAGER_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.MANAGER_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.MANAGER_SHEET.getIndex()
                ));
        predefinedList.add(
                new PredefinedDTO(
                        users,
                        1,
                        500,
                        ExcelImportTemplateColumn.Issue.REPORTER_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.REPORTER_SHEET.getCol(),
                        ExcelImportTemplateColumn.Issue.REPORTER_SHEET.getName(),
                        ExcelImportTemplateColumn.Issue.REPORTER_SHEET.getIndex()
                ));
        return predefinedList;
    }

    protected Map<String, Long> getManagers(Long projectId) {
        Map<String, Long> managerMap = new HashMap<>();
        ResponseEntity<Page<UserDTO>> response = baseFeignClient.listUsersByProjectId(projectId, 1, 0, null);
        List<UserDTO> users = response.getBody().getContent();
        users.forEach(u -> {
            if (u.getEnabled()) {
                String realName = u.getRealName();
                String loginName = u.getLoginName();
                String name = realName + "（" + loginName + "）";
                managerMap.put(name, u.getId());
            }
        });
        return managerMap;
    }

    protected Boolean setIssueCreateInfo(IssueCreateVO issueCreateVO,
                                         Long projectId,
                                         Map<String, IssueTypeVO> issueTypeMap,
                                         Map<String, Long> priorityMap,
                                         Map<String, Long> versionMap,
                                         Long userId,
                                         Map<String, Long> componentMap,
                                         Map<String, Long> sprintMap,
                                         Map<String, Long> managerMap,
                                         Integer rowNum,
                                         Sheet sheet,
                                         Map<Integer, Integer> sonParentMap,
                                         IssueTypeVO subTask,
                                         Map<String, Long> theSecondColumnMap) {
        issueCreateVO.setProjectId(projectId);
        Row row = sheet.getRow(rowNum);
        //经办人
        setManager(issueCreateVO, managerMap, row);
        //报告人
        setReporter(issueCreateVO, managerMap, row);
        //优先级
        String priorityName = row.getCell(ExcelImportTemplateColumn.Issue.PRIORITY_COL).toString();
        Long priorityId = priorityMap.get(priorityName);
        if (ObjectUtils.isEmpty(priorityId)) {
            return false;
        } else {
            issueCreateVO.setPriorityCode("priority" + priorityId);
            issueCreateVO.setPriorityId(priorityId);
        }
        //预估时间
        setRemainTime(issueCreateVO, row);
        //版本
        setVersion(issueCreateVO, versionMap, row);
        //描述
        setDescription(issueCreateVO, row);

        String typeName = getTypeName(row);
        if (isSubTask(row)) {
            //子任务是任务类型，无需设置故事点和史诗名
            String summary = row.getCell(ExcelImportTemplateColumn.Issue.SUB_TASK_COL).toString();
            if (!StringUtils.hasText(summary)) {
                throw new CommonException("error.summary.null");
            }
            issueCreateVO.setSummary(summary);
            issueCreateVO.setTypeCode(subTask.getTypeCode());
            issueCreateVO.setIssueTypeId(subTask.getId());
            //子任务的所属史诗模块和冲刺，保持与父节点统一
            Row parentRow = sheet.getRow(sonParentMap.get(rowNum));
            setBelongsEpic(issueCreateVO, parentRow, theSecondColumnMap, typeName);
            setComponent(issueCreateVO, parentRow, componentMap);
            setSprint(issueCreateVO, parentRow, sprintMap);
        } else {
            String summary = row.getCell(ExcelImportTemplateColumn.Issue.SUMMARY_COL).toString();
            if (!StringUtils.hasText(summary)) {
                throw new CommonException("error.summary.null");
            }
            issueCreateVO.setSummary(summary);
            IssueTypeVO issueType = issueTypeMap.get(typeName);
            if (issueType == null) {
                return false;
            }
            issueCreateVO.setTypeCode(issueType.getTypeCode());
            issueCreateVO.setIssueTypeId(issueType.getId());
            if (EPIC_CN.equals(typeName)) {
                //默认名称和概要相同
                String epicName = row.getCell(ExcelImportTemplateColumn.Issue.EPIC_NAME_COL).toString();
                issueCreateVO.setSummary(epicName);
                issueCreateVO.setEpicName(epicName);
            } else {
                if (STORY_CN.equals(typeName)) {
                    Cell storyPointCell = row.getCell(ExcelImportTemplateColumn.Issue.STORY_POINT_COL);
                    if (!isCellEmpty(storyPointCell)) {
                        issueCreateVO.setStoryPoints(new BigDecimal(storyPointCell.toString()));
                    }
                }
                setBelongsEpic(issueCreateVO, row, theSecondColumnMap, typeName);
            }
            setComponent(issueCreateVO, row, componentMap);
            setSprint(issueCreateVO, row, sprintMap);
        }
        return true;
    }

    protected void setDescription(IssueCreateVO issueCreateVO, Row row) {
        Cell descriptionCell = row.getCell(ExcelImportTemplateColumn.Issue.DESCRIPTION_COL);
        if (!isCellEmpty(descriptionCell)) {
            String description = descriptionCell.toString();
            if (StringUtils.hasText(description)) {
                issueCreateVO.setDescription("[{\"insert\":\"" + StringUtil.replaceChar(description) + "\\n\"}]");
            }
        }
    }

    protected void setVersion(IssueCreateVO issueCreateVO, Map<String, Long> versionMap, Row row) {
        Cell versionCell = row.getCell(ExcelImportTemplateColumn.Issue.FIX_VERSION_COL);
        if (!isCellEmpty(versionCell)) {
            String version = versionCell.toString();
            if (StringUtils.hasText(version)) {
                List<VersionIssueRelVO> versionIssueRelList = new ArrayList<>();
                VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
                versionIssueRelVO.setVersionId(versionMap.get(version));
                versionIssueRelVO.setRelationType(RELATION_TYPE_FIX);
                versionIssueRelList.add(versionIssueRelVO);
                issueCreateVO.setVersionIssueRelVOList(versionIssueRelList);
            }
        }
    }

    protected void setRemainTime(IssueCreateVO issueCreateVO, Row row) {
        Cell remainTimeCell = row.getCell(ExcelImportTemplateColumn.Issue.REMAIN_TIME_COL);
        if (!isCellEmpty(remainTimeCell)) {
            issueCreateVO.setRemainingTime(new BigDecimal(remainTimeCell.toString()));
        }
    }

    protected void setManager(IssueCreateVO issueCreateVO, Map<String, Long> managerMap, Row row) {
        Cell cell = row.getCell(ExcelImportTemplateColumn.Issue.MANAGER_COL);
        if (!isCellEmpty(cell)) {
            String manager = cell.toString();
            if (StringUtils.hasText(manager)) {
                Long assigneeId = managerMap.get(manager);
                issueCreateVO.setAssigneeId(assigneeId);
            }
        }
    }

    protected void setReporter(IssueCreateVO issueCreateVO, Map<String, Long> managerMap, Row row) {
        Cell cell = row.getCell(ExcelImportTemplateColumn.Issue.REPORTER_COL);
        if (!isCellEmpty(cell)) {
            String manager = cell.toString();
            if (StringUtils.hasText(manager)) {
                Long reporterId = managerMap.get(manager);
                issueCreateVO.setReporterId(reporterId);
            }
        }
    }

    protected void setSprint(IssueCreateVO issueCreateVO, Row row, Map<String, Long> sprintMap) {
        Cell sprintCell = row.getCell(ExcelImportTemplateColumn.Issue.SPRINT_COL);
        if (!isCellEmpty(sprintCell)) {
            String sprint = sprintCell.toString();
            if (StringUtils.hasText(sprint)) {
                issueCreateVO.setSprintId(sprintMap.get(sprint));
            }
        }
    }

    protected void setComponent(IssueCreateVO issueCreateVO, Row row, Map<String, Long> componentMap) {
        Cell componentCell = row.getCell(ExcelImportTemplateColumn.Issue.COMPONENT_COL);
        if (!isCellEmpty(componentCell)) {
            String value = componentCell.toString();
            if (StringUtils.hasText(value)) {
                ComponentIssueRelVO componentIssueRelVO = new ComponentIssueRelVO();
                componentIssueRelVO.setComponentId(componentMap.get(value));
                issueCreateVO.setComponentIssueRelVOList(Arrays.asList(componentIssueRelVO));
            }
        }
    }

    protected void setBelongsEpic(IssueCreateVO issueCreateVO, Row row,
                                  Map<String, Long> theSecondColumnMap,
                                  String typeName) {
        Cell cell = row.getCell(ExcelImportTemplateColumn.Issue.EPIC_COL);
        if (!isCellEmpty(cell)) {
            String belongsEpic = cell.toString();
            //子任务不设置史诗
            if (StringUtils.hasText(belongsEpic) && !SUB_TASK_CN.equals(typeName)) {
                issueCreateVO.setEpicId(theSecondColumnMap.get(belongsEpic));
            }
        }
    }

    protected void updateFinalRecode(FileOperationHistoryDTO fileOperationHistoryDTO, Long successcount, Long failCount, String status) {
        FileOperationHistoryDTO update = new FileOperationHistoryDTO();
        update.setId(fileOperationHistoryDTO.getId());
        update.setSuccessCount(successcount);
        update.setFailCount(failCount);
        update.setStatus(status);
        update.setFileUrl(fileOperationHistoryDTO.getFileUrl());
        update.setObjectVersionNumber(fileOperationHistoryDTO.getObjectVersionNumber());
        if (fileOperationHistoryMapper.updateByPrimaryKeySelective(update) != 1) {
            throw new CommonException("error.FileOperationHistoryDTO.update");
        }
        FileOperationHistoryDTO result = fileOperationHistoryMapper.selectByPrimaryKey(update.getId());
        sendProcess(result, result.getUserId(), 1.0);
    }

    protected IssueTypeVO setIssueTypeAndPriorityMap(Long organizationId,
                                                     Long projectId,
                                                     Map<String, IssueTypeVO> issueTypeMap,
                                                     Map<String, Long> priorityMap,
                                                     List<String> issueTypeList,
                                                     List<String> priorityList,
                                                     boolean withFeature) {
        IssueTypeVO subTask = null;
        List<PriorityVO> priorityVOList = priorityService.queryByOrganizationIdList(organizationId);
        List<IssueTypeVO> issueTypeVOList = projectConfigService.queryIssueTypesByProjectId(projectId, APPLY_TYPE_AGILE);
        for (PriorityVO priorityVO : priorityVOList) {
            if (priorityVO.getEnable()) {
                priorityMap.put(priorityVO.getName(), priorityVO.getId());
                priorityList.add(priorityVO.getName());
            }
        }
        for (IssueTypeVO issueTypeVO : issueTypeVOList) {
            if (SUB_TASK.equals(issueTypeVO.getTypeCode())) {
                subTask = issueTypeVO;
            }
            //有特性列跳过史诗类型
            if (withFeature && "issue_epic".equals(issueTypeVO.getTypeCode())) {
                continue;
            }
            if (!SUB_TASK.equals(issueTypeVO.getTypeCode()) && !FEATURE.equals(issueTypeVO.getTypeCode())) {
                issueTypeMap.put(issueTypeVO.getName(), issueTypeVO);
            }
        }
        issueTypeList.addAll(issueTypeMap.keySet());
        return subTask;
    }

    @Override
    public void sendProcess(FileOperationHistoryDTO fileOperationHistoryDTO, Long userId, Double process) {
        fileOperationHistoryDTO.setProcess(process);
        String message = null;
        try {
            message = objectMapper.writeValueAsString(fileOperationHistoryDTO);
        } catch (JsonProcessingException e) {
            LOGGER.error("object to json error: {}", e);
        }
        switch (fileOperationHistoryDTO.getAction()) {
            case UPLOAD_FILE:
                messageClient.sendByUserId(userId, WEBSOCKET_IMPORT_CODE, message);
                break;
            case DOWNLOAD_FILE:
            case DOWNLOAD_FILE_ISSUE_ANALYSIS:
            case DOWNLOAD_FILE_PI:
                messageClient.sendByUserId(userId, WEBSOCKET_EXPORT_CODE, message);
                break;
        }
    }

    protected String uploadErrorExcel(Workbook errorWorkbook, Long organizationId) {
        // 上传错误的excel
        MultipartFile multipartFile = new MultipartExcelUtil(MULTIPART_NAME, ORIGINAL_FILE_NAME, errorWorkbook);
        return fileClient.uploadFile(organizationId, BACKETNAME, null, FILE_NAME, multipartFile);
    }

    protected Boolean checkEpicNameExist(Long projectId, String epicName) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setEpicName(epicName);
        List<IssueDTO> issueDTOList = issueMapper.select(issueDTO);
        return issueDTOList == null || issueDTOList.isEmpty();
    }

    protected Map<Integer, String> checkRule(Long projectId, Sheet sheet,
                                             List<String> issueTypeList,
                                             List<String> priorityList,
                                             List<String> versionList,
                                             List<String> componentList,
                                             List<String> sprintList,
                                             int rowNum,
                                             Set<Integer> illegalRow,
                                             Set<String> theSecondColumn,
                                             List<String> managers) {
        Row row = sheet.getRow(rowNum);
        Map<Integer, String> errorMessage = new HashMap<>();
        // 经办人,非必填
        checkUser(managers, row, errorMessage, ExcelImportTemplateColumn.Issue.MANAGER_COL, "经办人输入错误");
        checkUser(managers, row, errorMessage, ExcelImportTemplateColumn.Issue.REPORTER_COL, "报告人输入错误");
        //优先级
        checkPriority(priorityList, row, errorMessage);
        //预估时间
        checkRemainTime(row, errorMessage);
        //版本
        checkVersion(versionList, row, errorMessage);
        //故事点
        checkStoryPoint(row, errorMessage);

        if (isSubTask(row)) {
            //子任务只校验子任务概述列
            String subTaskSummary = row.getCell(5).toString();
            if (illegalRow.contains(rowNum)) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.ISSUE_TYPE_COL, "子任务必须有父节点");
            } else if (subTaskSummary.length() > 44) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.SUB_TASK_COL, "子任务概要过长");
            }
        } else {
            Cell issueTypeCell = row.getCell(0);
            //问题类型
            if (isCellEmpty(issueTypeCell)) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.ISSUE_TYPE_COL, "问题类型不能为空");
            } else if (!issueTypeList.contains(issueTypeCell.toString())) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.ISSUE_TYPE_COL, "问题类型输入错误");
            } else if (EPIC_CN.equals(issueTypeCell.toString())) {
                //如果是史诗的话，判断是否重复和字段长度
                Cell epicNameCell = row.getCell(ExcelImportTemplateColumn.Issue.EPIC_NAME_COL);
                if (isCellEmpty(epicNameCell)) {
                    errorMessage.put(ExcelImportTemplateColumn.Issue.EPIC_NAME_COL, "史诗名称不能为空");
                } else {
                    String epicName = epicNameCell.toString().trim();
                    if (epicName.length() > 20) {
                        errorMessage.put(ExcelImportTemplateColumn.Issue.EPIC_NAME_COL, "史诗名称过长");
                    } else if (!checkEpicNameExist(projectId, epicName)) {
                        errorMessage.put(ExcelImportTemplateColumn.Issue.EPIC_NAME_COL, "史诗名称重复");
                    }
                }
            }
            //检查第二列，史诗
            checkSecondColumn(theSecondColumn, row, errorMessage);
            //模块
            checkComponent(componentList, row, errorMessage);
            //冲刺
            checkSprint(sprintList, row, errorMessage);
            Cell summaryCell = row.getCell(ExcelImportTemplateColumn.Issue.SUMMARY_COL);
            if (isCellEmpty(summaryCell)) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.SUMMARY_COL, "概要不能为空");
            } else if (summaryCell.toString().length() > 44) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.SUMMARY_COL, "概要过长");
            }
        }
        return errorMessage;
    }

    protected void checkSprint(List<String> sprintList, Row row, Map<Integer, String> errorMessage) {
        Cell sprintCell = row.getCell(ExcelImportTemplateColumn.Issue.SPRINT_COL);
        if (!isCellEmpty(sprintCell) && !sprintList.contains(sprintCell.toString())) {
            errorMessage.put(ExcelImportTemplateColumn.Issue.SPRINT_COL, "请输入正确的冲刺");
        }
    }

    protected void checkComponent(List<String> componentList, Row row, Map<Integer, String> errorMessage) {
        Cell componentCell = row.getCell(ExcelImportTemplateColumn.Issue.COMPONENT_COL);
        if (!isCellEmpty(componentCell) && !componentList.contains(componentCell.toString())) {
            errorMessage.put(ExcelImportTemplateColumn.Issue.COMPONENT_COL, "请输入正确的模块");
        }
    }

    protected void checkSecondColumn(Set<String> theSecondColumn, Row row, Map<Integer, String> errorMessage) {
        Cell secondColumnCell = row.getCell(ExcelImportTemplateColumn.Issue.EPIC_COL);
        if (!isCellEmpty(secondColumnCell) && !theSecondColumn.contains(secondColumnCell.toString())) {
            errorMessage.put(ExcelImportTemplateColumn.Issue.EPIC_COL, "所属史诗输入错误");
        }
    }

    protected void checkStoryPoint(Row row, Map<Integer, String> errorMessage) {
        Cell storyPointCell = row.getCell(ExcelImportTemplateColumn.Issue.STORY_POINT_COL);
        if (!isCellEmpty(storyPointCell)) {
            String storyPointStr = storyPointCell.toString().trim();
            if (storyPointStr.length() > 3) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.STORY_POINT_COL, "请输入正确的位数");
            } else if (!NumberUtil.isNumeric(storyPointStr)) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.STORY_POINT_COL, "请输入数字");
            } else {
                if (NumberUtil.isInteger(storyPointStr) || NumberUtil.canParseInteger(storyPointStr)) {
                    if (storyPointStr.trim().length() > 3) {
                        errorMessage.put(ExcelImportTemplateColumn.Issue.STORY_POINT_COL, "最大支持3位整数");
                    } else if (storyPointStr.trim().length() > 1 && "0".equals(storyPointStr.trim().substring(0, 0))) {
                        errorMessage.put(ExcelImportTemplateColumn.Issue.STORY_POINT_COL, "请输入正确的整数");
                    }
                } else if (!"0.5".equals(storyPointStr)) {
                    errorMessage.put(ExcelImportTemplateColumn.Issue.STORY_POINT_COL, "小数只支持0.5");
                }
            }
        }
    }

    protected void checkVersion(List<String> versionList, Row row, Map<Integer, String> errorMessage) {
        Cell versionCell = row.getCell(ExcelImportTemplateColumn.Issue.FIX_VERSION_COL);
        if (!isCellEmpty(versionCell)) {
            if (!versionList.contains(versionCell.toString())) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.FIX_VERSION_COL, "请输入正确的版本");
            }
        }
    }

    protected void checkRemainTime(Row row, Map<Integer, String> errorMessage) {
        Cell remainTimeCell = row.getCell(ExcelImportTemplateColumn.Issue.REMAIN_TIME_COL);
        if (!isCellEmpty(remainTimeCell)) {
            String remainTime = remainTimeCell.toString().trim();
            if (remainTime.length() > 3) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.REMAIN_TIME_COL, "请输入正确的位数");
            } else if (!NumberUtil.isNumeric(remainTime)) {
                errorMessage.put(ExcelImportTemplateColumn.Issue.REMAIN_TIME_COL, "请输入数字");
            } else {
                if (NumberUtil.isInteger(remainTime) || NumberUtil.canParseInteger(remainTime)) {
                    if (remainTime.length() > 3) {
                        errorMessage.put(ExcelImportTemplateColumn.Issue.REMAIN_TIME_COL, "最大支持3位整数");
                    } else if (remainTime.length() > 1 && "0".equals(remainTime.substring(0, 0))) {
                        errorMessage.put(ExcelImportTemplateColumn.Issue.REMAIN_TIME_COL, "请输入正确的整数");
                    }
                } else if (!"0.5".equals(remainTime)) {
                    errorMessage.put(ExcelImportTemplateColumn.Issue.REMAIN_TIME_COL, "小数只支持0.5");
                }
            }
        }
    }

    protected void checkPriority(List<String> priorityList, Row row, Map<Integer, String> errorMessage) {
        Cell priorityCell = row.getCell(ExcelImportTemplateColumn.Issue.PRIORITY_COL);
        if (isCellEmpty(priorityCell)) {
            errorMessage.put(ExcelImportTemplateColumn.Issue.PRIORITY_COL, "优先级不能为空");
        } else if (!priorityList.contains(priorityCell.toString())) {
            errorMessage.put(ExcelImportTemplateColumn.Issue.PRIORITY_COL, "优先级输入错误");
        }
    }

    protected void checkUser(List<String> managers, Row row, Map<Integer, String> errorMessage,
                             int col, String msg) {
        Cell managerCell = row.getCell(col);
        if (!isCellEmpty(managerCell)) {
            String manager = managerCell.toString();
            if (!managers.contains(manager)) {
                errorMessage.put(col, msg);
            }
        }
    }

    protected boolean isSubTask(Row row) {
        return SUB_TASK_CN.equals(getTypeName(row));
    }

    protected Boolean checkCanceled(Long projectId, Long fileOperationHistoryId, List<Long> importedIssueIds) {
        FileOperationHistoryDTO checkCanceledDO = fileOperationHistoryMapper.selectByPrimaryKey(fileOperationHistoryId);
        if (UPLOAD_FILE.equals(checkCanceledDO.getAction()) && CANCELED.equals(checkCanceledDO.getStatus())) {
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
        return cell == null || cell.toString().equals("") || cell.getCellType() == XSSFCell.CELL_TYPE_BLANK;
    }

    @Async
    @Override
    public void batchImport(Long projectId, Long organizationId, Long userId, Workbook workbook) {
        String status = DOING;
        FileOperationHistoryDTO res = initFileOperationHistory(projectId, userId, status, UPLOAD_FILE);
        validateWorkbook(projectId, userId, workbook, res, FIELDS_NAME);

        Sheet sheet = workbook.getSheetAt(1);
        // 获取所有非空行
        int columnNum = FIELDS_NAME.length;
        Integer allRowCount = getRealRowCount(sheet, columnNum);
        // 查询组织下的优先级与问题类型
        Map<String, IssueTypeVO> issueTypeMap = new HashMap<>();
        Map<String, Long> priorityMap = new HashMap<>();
        List<String> issueTypeList = new ArrayList<>();
        List<String> priorityList = new ArrayList<>();
        IssueTypeVO subTask = setIssueTypeAndPriorityMap(organizationId, projectId, issueTypeMap, priorityMap, issueTypeList, priorityList, false);
        Long failCount = 0L;
        Long successCount = 0L;
        Integer processNum = 0;
        List<Integer> errorRows = new ArrayList<>();
        Map<Integer, List<Integer>> errorMapList = new HashMap<>();
        Map<String, Long> versionMap = new HashMap<>();
        Map<String, Long> componentMap = new HashMap<>();
        Map<String, Long> sprintMap = new HashMap<>();
        List<ProductVersionCommonDTO> productVersionCommonDTOList = productVersionMapper.listByProjectId(projectId);
        List<IssueComponentDTO> issueComponentDTOList = issueComponentMapper.selectByProjectId(projectId);
        List<SprintDTO> sprintDTOList = sprintMapper.selectNotDoneByProjectId(projectId);
        List<String> versionList = new ArrayList<>();
        for (ProductVersionCommonDTO productVersionCommonDTO : productVersionCommonDTOList) {
            if (VERSION_PLANNING.equals(productVersionCommonDTO.getStatusCode())) {
                versionMap.put(productVersionCommonDTO.getName(), productVersionCommonDTO.getVersionId());
                versionList.add(productVersionCommonDTO.getName());
            }
        }
        List<String> componentList = new ArrayList<>();
        for (IssueComponentDTO issueComponentDTO : issueComponentDTOList) {
            componentList.add(issueComponentDTO.getName());
            componentMap.put(issueComponentDTO.getName(), issueComponentDTO.getComponentId());
        }
        List<String> sprintList = new ArrayList<>();
        for (SprintDTO sprintDTO : sprintDTOList) {
            sprintList.add(sprintDTO.getSprintName());
            sprintMap.put(sprintDTO.getSprintName(), sprintDTO.getSprintId());
        }
        //第二列为所属史诗
        Map<String, Long> theSecondColumnMap = getEpicMap(projectId);

        Map<String, Long> managerMap = getManagers(projectId);
        List<String> managers = new ArrayList<>(managerMap.keySet());

        List<Long> importedIssueIds = new ArrayList<>();

        Map<Integer, String> allIssueType = new LinkedHashMap<>();
        List<IssueTypeLinkDTO> issueTypeLinks = getAllIssueTypeLinks(allRowCount, sheet, columnNum, allIssueType);
        Map<Integer, Set<Integer>> parentSonMap = getParentSonMap(issueTypeLinks);
        Map<Integer, Integer> sonParentMap = getSonParentMap(parentSonMap);
        //获取无父节点的子任务
        Set<Integer> illegalRow = getIllegalRow(allIssueType, sonParentMap);

        for (int r = 1; r <= allRowCount; r++) {
            if (checkCanceled(projectId, res.getId(), importedIssueIds)) {
                return;
            }
            Row row = sheet.getRow(r);
            if (isSkip(row, columnNum)) {
                continue;
            }
            for (int w = 0; w < columnNum; w++) {
                if (row.getCell(w) != null) {
                    row.getCell(w).setCellType(XSSFCell.CELL_TYPE_STRING);
                }
            }

            String typeName = allIssueType.get(r);
            //有子节点的故事和任务，要和子节点一块校验，有一个不合法，则全为错误的
            Set<Integer> set = parentSonMap.get(r);
            Boolean hasSonNodes = (set != null && !set.isEmpty());
            if ((STORY_CN.equals(typeName)
                    || TASK_CN.equals(typeName)
                    || BUG_CN.equals(typeName))
                    && hasSonNodes) {
                Map<String, Object> returnMap = batchCheck(projectId, sheet, issueTypeList, priorityList,
                        versionList, issueTypeMap, componentList, sprintList, r, illegalRow, set, columnNum,
                        theSecondColumnMap.keySet(), managers);
                Map<Integer, Map<Integer, String>> errorMaps = (Map<Integer, Map<Integer, String>>) returnMap.get("errorMap");
                set = (Set<Integer>) returnMap.get("sonSet");
                if (!errorMaps.isEmpty()) {
                    int size = errorMaps.size();
                    failCount = failCount + size;
                    for (Map.Entry<Integer, Map<Integer, String>> entry : errorMaps.entrySet()) {
                        int rowNum = entry.getKey();
                        Map<Integer, String> errorMap = entry.getValue();
                        processErrorMap(errorMapList, rowNum, sheet.getRow(rowNum), errorMap, errorRows);
                    }
                    res.setFailCount(failCount);
                    processNum = processNum + size;
                    sendProcess(res, userId, processNum * 1.0 / allRowCount);
                    //设置for循环的指针为子节点的最大行数
                    r = Collections.max(set);
                    continue;
                }

                Set<Long> insertIds = batchInsert(projectId, r, issueTypeMap, priorityMap, versionMap,
                        userId, componentMap, sprintMap, sheet, set, managerMap, sonParentMap, subTask, theSecondColumnMap);
                if (insertIds.isEmpty()) {
                    failCount = failCount + set.size() + 1;
                    errorRows.add(r);
                    errorRows.addAll(set);
                } else {
                    importedIssueIds.addAll(insertIds);
                    successCount = successCount + insertIds.size();
                }
                r = Collections.max(set);
            } else {
                Map<Integer, String> errorMap = checkRule(projectId, sheet, issueTypeList, priorityList,
                        versionList, componentList, sprintList, r, illegalRow, theSecondColumnMap.keySet(), managers);
                if (!errorMap.isEmpty()) {
                    failCount++;
                    processErrorMap(errorMapList, r, row, errorMap, errorRows);
                    res.setFailCount(failCount);
                    processNum++;
                    sendProcess(res, userId, processNum * 1.0 / allRowCount);
                    continue;
                }
                IssueCreateVO issueCreateVO = new IssueCreateVO();
                Boolean ok = setIssueCreateInfo(issueCreateVO, projectId, issueTypeMap, priorityMap,
                        versionMap, userId, componentMap, sprintMap, managerMap, r, sheet, sonParentMap,
                        subTask, theSecondColumnMap);

                IssueVO result = null;
                if (ok) {
                    result = stateMachineClientService.createIssue(issueCreateVO, APPLY_TYPE_AGILE);
                }
                if (result == null) {
                    failCount++;
                    errorRows.add(row.getRowNum());
                } else {
                    importedIssueIds.add(result.getIssueId());
                    successCount++;
                }
            }
            processNum++;
            res.setFailCount(failCount);
            res.setSuccessCount(successCount);
            sendProcess(res, userId, processNum * 1.0 / allRowCount);
        }


        if (!errorRows.isEmpty()) {
            LOGGER.info("导入数据有误");
            PredefinedDTO theSecondColumnPredefined = getEpicPredefined(projectId);
            Workbook result = ExcelUtil.generateExcelAwesome(workbook, errorRows,
                    errorMapList, FIELDS_NAME, priorityList, issueTypeList, versionList,
                    IMPORT_TEMPLATE_NAME, componentList, sprintList, managers,
                    theSecondColumnPredefined, false);
            String errorWorkBookUrl = uploadErrorExcel(result, organizationId);
            res.setFileUrl(errorWorkBookUrl);
            status = FAILED;
        } else {
            status = SUCCESS;
        }
        updateFinalRecode(res, successCount, failCount, status);
    }

    protected Set<Long> batchInsert(Long projectId, int rowNum, Map<String, IssueTypeVO> issueTypeMap,
                                    Map<String, Long> priorityMap, Map<String, Long> versionMap,
                                    Long userId, Map<String, Long> componentMap, Map<String, Long> sprintMap,
                                    Sheet sheet, Set<Integer> set, Map<String, Long> managerMap,
                                    Map<Integer, Integer> sonParentMap, IssueTypeVO subTask,
                                    Map<String, Long> theSecondColumnMap) {
        Set<Long> issueIds = new HashSet<>();
        //插入父节点
        IssueCreateVO issueCreateVO = new IssueCreateVO();

        Boolean ok = setIssueCreateInfo(issueCreateVO, projectId, issueTypeMap, priorityMap,
                versionMap, userId, componentMap, sprintMap, managerMap, rowNum, sheet, sonParentMap, subTask, theSecondColumnMap);
        IssueVO parent = null;
        if (ok) {
            parent = stateMachineClientService.createIssue(issueCreateVO, APPLY_TYPE_AGILE);
        }
        if (parent == null) {
            return issueIds;
        }
        Long parentId = parent.getIssueId();
        issueIds.add(parentId);
        //处理子节点
        set.forEach(s -> {
            IssueCreateVO issueCreate = new IssueCreateVO();
            Boolean success = setIssueCreateInfo(issueCreate, projectId, issueTypeMap, priorityMap,
                    versionMap, userId, componentMap, sprintMap, managerMap, s, sheet, sonParentMap, subTask, theSecondColumnMap);
            if (success) {
                String typeCode = issueCreate.getTypeCode();
                if (SUB_TASK.equals(typeCode)) {
                    issueCreate.setParentIssueId(parentId);
                }
                if (issueTypeMap.get(BUG_CN).getTypeCode().equals(typeCode)) {
                    issueCreate.setRelateIssueId(parentId);
                }
                IssueVO result = stateMachineClientService.createIssue(issueCreate, APPLY_TYPE_AGILE);
                if (result != null) {
                    issueIds.add(result.getIssueId());
                }
            }
        });
        if (set.size() + 1 == issueIds.size()) {
            return issueIds;
        } else {
            return new HashSet<>();
        }
    }

    protected Map<String, Object> batchCheck(Long projectId, Sheet sheet, List<String> issueTypeList,
                                             List<String> priorityList, List<String> versionList, Map<String, IssueTypeVO> issueTypeMap,
                                             List<String> componentList, List<String> sprintList, int rowNum,
                                             Set<Integer> illegalRow, Set<Integer> sonSet, int columnNum,
                                             Set<String> theSecondColumn, List<String> managers) {
        //key为row,value为错误信息
        Map<Integer, Map<Integer, String>> map = new HashMap<>();
        //key为列，value为错误详情，先判断父节点
        Map<Integer, String> errorMap = checkRule(projectId, sheet, issueTypeList, priorityList,
                versionList, componentList, sprintList, rowNum, illegalRow, theSecondColumn, managers);
        Set<Integer> newSet = new HashSet<>();
        if (!errorMap.isEmpty()) {
            map.put(rowNum, errorMap);
            newSet.addAll(sonSet);
        } else {
            sonSet.forEach(r -> {
                Row row = sheet.getRow(r);
                if (isSkip(row, columnNum)) {
                    return;
                }
                for (int w = 0; w < FIELDS_NAME.length; w++) {
                    if (row.getCell(w) != null) {
                        row.getCell(w).setCellType(XSSFCell.CELL_TYPE_STRING);
                    }
                }
                newSet.add(r);
                Map<Integer, String> error = checkRule(projectId, sheet, issueTypeList, priorityList,
                        versionList, componentList, sprintList, r, illegalRow, theSecondColumn, managers);
                if (!error.isEmpty()) {
                    map.put(r, error);
                }
            });
        }
        //如果有一行有问题，全部置为失败
        if (!map.isEmpty()) {
            fillInErrorMap(map, rowNum);
            newSet.forEach(n -> fillInErrorMap(map, n));
        }
        Map<String, Object> result = new HashMap<>();
        result.put("errorMap", map);
        result.put("sonSet", newSet);

        return result;
    }

    protected void fillInErrorMap(Map<Integer, Map<Integer, String>> map, int rowNum) {
        Map<Integer, String> error = map.get(rowNum);
        if (ObjectUtils.isEmpty(error)) {
            error = new HashMap<>();
            map.put(rowNum, error);
        }
        error.put(5, "父子结构中有错误数据");
    }

    protected Set<Integer> getIllegalRow(Map<Integer, String> allIssueType, Map<Integer, Integer> sonParentMap) {
        Set<Integer> set = new HashSet<>();
        for (Map.Entry<Integer, String> entry : allIssueType.entrySet()) {
            Integer key = entry.getKey();
            String value = entry.getValue();
            if (SUB_TASK_CN.equals(value)) {
                //无父节点
                if (sonParentMap.get(key) == null) {
                    set.add(key);
                }
            }
        }
        return set;
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

    protected Map<Integer, Set<Integer>> getParentSonMap(List<IssueTypeLinkDTO> issueTypeLinks) {
        Map<Integer, Set<Integer>> map = new HashMap<>();
        for (IssueTypeLinkDTO issueTypeLink : issueTypeLinks) {
            Integer row = issueTypeLink.getRow();
            String type = issueTypeLink.getType();
            //故事下只有子任务
            if (STORY_CN.equals(type)) {
                storyRecursive(map, issueTypeLink, row);
            }
            //任务或缺陷下的子任务
            if (TASK_CN.equals(type) || BUG_CN.equals(type)) {
                taskRecursive(map, issueTypeLink, row);
            }
        }
        return map;
    }

    private void taskRecursive(Map<Integer, Set<Integer>> map, IssueTypeLinkDTO issueTypeLink, Integer row) {
        if (issueTypeLink.hasNext()) {
            IssueTypeLinkDTO next = issueTypeLink.getNext();
            String nextType = next.getType();
            Integer nextRow = next.getRow();
            if (SUB_TASK_CN.equals(nextType)) {
                processSonRow(map, row, nextRow);
                taskRecursive(map, next, row);
            }
        }
    }

    private void processSonRow(Map<Integer, Set<Integer>> map, Integer row, Integer nextRow) {
        Set<Integer> set = map.get(row);
        if (set == null) {
            set = new HashSet<>();
            set.add(nextRow);
            map.put(row, set);
        } else {
            set.add(nextRow);
        }
    }

    private void storyRecursive(Map<Integer, Set<Integer>> map, IssueTypeLinkDTO issueTypeLink,
                                Integer row) {
        if (issueTypeLink.hasNext()) {
            IssueTypeLinkDTO next = issueTypeLink.getNext();
            String nextType = next.getType();
            Integer nextRow = next.getRow();
            if (SUB_TASK_CN.equals(nextType)) {
                processSonRow(map, row, nextRow);
                storyRecursive(map, next, row);
            }
        }
    }

    protected List<IssueTypeLinkDTO> getAllIssueTypeLinks(Integer allRowCount, Sheet sheet, int columnNum, Map<Integer, String> allIssueType) {
        List<IssueTypeLinkDTO> issueTypeLinks = new ArrayList<>();
        for (int i = 1; i <= allRowCount; i++) {
            int size = issueTypeLinks.size();
            IssueTypeLinkDTO lastIssueTypeLink = null;
            if (size > 0) {
                lastIssueTypeLink = issueTypeLinks.get(size - 1);
            }
            Row row = sheet.getRow(i);
            if (isSkip(row, columnNum)) {
                continue;
            }
            String type = getTypeName(row);
            if (type == null) {
                continue;
            }
            IssueTypeLinkDTO issueTypeLink = new IssueTypeLinkDTO(i, type);
            issueTypeLinks.add(issueTypeLink);
            if (lastIssueTypeLink != null) {
                lastIssueTypeLink.setNext(issueTypeLink);
            }
            allIssueType.put(i, type);
        }
        return issueTypeLinks;
    }

    protected String getTypeName(Row row) {
        Cell issueTypeCell = row.getCell(0);
        Cell subTaskCell = row.getCell(5);
        if (isCellEmpty(issueTypeCell) && !isCellEmpty(subTaskCell)) {
            return SUB_TASK_CN;
        } else if (!isCellEmpty(issueTypeCell)) {
            return issueTypeCell.toString();
        } else {
            return null;
        }
    }

    protected void processErrorMap(Map<Integer, List<Integer>> errorMapList,
                                   int r, Row row, Map<Integer, String> errorMap,
                                   List<Integer> errorRows) {
        Iterator<Map.Entry<Integer, String>> entries = errorMap.entrySet().iterator();
        while (entries.hasNext()) {
            Map.Entry<Integer, String> entry = entries.next();
            Integer key = entry.getKey();
            String value = entry.getValue();
            if (row.getCell(key) == null) {
                row.createCell(key).setCellValue("(" + value + ")");
            } else {
                row.getCell(key).setCellValue(row.getCell(key).toString() + " (" + value + ")");
            }

            List<Integer> cList = errorMapList.get(r);
            if (cList == null) {
                cList = new ArrayList<>();
            }
            cList.add(key);
            errorMapList.put(r, cList);
        }
        errorRows.add(row.getRowNum());
    }

    protected void validateWorkbook(Long projectId, Long userId, Workbook workbook, FileOperationHistoryDTO res,
                                    String[] headers) {
        if (workbook.getActiveSheetIndex() < 1
                || workbook.getSheetAt(1) == null
                || workbook.getSheetAt(1).getSheetName() == null
                || !IMPORT_TEMPLATE_NAME.equals(workbook.getSheetAt(1).getSheetName())
                || isOldExcel(workbook, headers)) {
            if (fileOperationHistoryMapper.updateByPrimaryKeySelective(new FileOperationHistoryDTO(projectId, res.getId(), UPLOAD_FILE, "template_error", res.getObjectVersionNumber())) != 1) {
                throw new CommonException("error.FileOperationHistoryDTO.update");
            }
            FileOperationHistoryDTO errorImport = fileOperationHistoryMapper.selectByPrimaryKey(res.getId());
            sendProcess(errorImport, userId, 0.0);
            throw new CommonException("error.sheet.import");
        }
    }

    private boolean isOldExcel(Workbook workbook, String[] headers) {
        //判断是否为旧模版
        Sheet sheet = workbook.getSheetAt(1);
        Row headerRow = sheet.getRow(0);
        if (headerRow == null) {
            return true;
        }
        for (int i = 0; i < headers.length; i++) {
            String header = headers[i];
            Cell cell = headerRow.getCell(i);
            if (isCellEmpty(cell)) {
                return true;
            }
            if (!header.equals(cell.toString())) {
                return true;
            }
        }
        return false;
    }

    @Override
    public FileOperationHistoryDTO initFileOperationHistory(Long projectId, Long userId, String status, String action) {
        FileOperationHistoryDTO fileOperationHistoryDTO = new FileOperationHistoryDTO(projectId, userId, action, 0L, 0L, status);
        if (fileOperationHistoryMapper.insert(fileOperationHistoryDTO) != 1) {
            throw new CommonException("error.FileOperationHistoryDTO.insert");
        }
        FileOperationHistoryDTO res = fileOperationHistoryMapper.selectByPrimaryKey(fileOperationHistoryDTO.getId());
        sendProcess(res, userId, 0.0);
        return res;
    }


    @Override
    public void cancelImport(Long projectId, Long id, Long objectVersionNumber) {
        FileOperationHistoryDTO fileOperationHistoryDTO = new FileOperationHistoryDTO();
        fileOperationHistoryDTO.setId(id);
        fileOperationHistoryDTO.setStatus(CANCELED);
        fileOperationHistoryDTO.setObjectVersionNumber(objectVersionNumber);
        if (fileOperationHistoryMapper.updateByPrimaryKeySelective(fileOperationHistoryDTO) != 1) {
            throw new CommonException("error.FileOperationHistoryDTO.update");
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
    public void asyncExportIssues(Long projectId, SearchVO searchVO, HttpServletRequest request,
                                  HttpServletResponse response, Long organizationId, Sort sort, ServletRequestAttributes requestAttributes) {
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        FileOperationHistoryDTO fileOperationHistoryDTO = initFileOperationHistory(projectId, userId, DOING, DOWNLOAD_FILE);
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

        String sheetName = project.getName();
        Workbook workbook = ExcelUtil.initIssueExportWorkbook(sheetName, fieldNames);
        ExcelCursorDTO cursor = new ExcelCursorDTO(1, 0, 1000);
        if (condition) {
            String filterSql = null;
            if (searchVO.getQuickFilterIds() != null && !searchVO.getQuickFilterIds().isEmpty()) {
                filterSql = getQuickFilter(searchVO.getQuickFilterIds());
            }
            final String searchSql = filterSql;
            String orderStr = getOrderStrOfQueryingIssuesWithSub(sort);
            while (true) {
                //查询所有父节点问题
                Page<IssueDTO> page =
                        PageHelper.doPage(cursor.getPage(), cursor.getSize(), () -> issueMapper.queryIssueIdsListWithSub(projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds(), orderStr));
                if (page.getTotalElements() < 1) {
                    break;
                }
                List<Long> parentIds = page.getContent().stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
                List<Long> issueIds = new ArrayList<>();
                Map<Long, Set<Long>> parentSonMap = new HashMap<>();
                List<IssueDTO> issues = new ArrayList<>();
                if (!parentIds.isEmpty()) {
                    Set<Long> childrenIds = issueMapper.queryChildrenIdByParentId(parentIds, projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds());
                    cursor.addCollections(childrenIds);
                    issues = issueMapper.queryIssueListWithSubByIssueIds(parentIds, childrenIds, true);
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
                        if (!ObjectUtils.isEmpty(assigneeId) && !Objects.equals(assigneeId, 0L)) {
                            userIds.add(assigneeId);
                        }
                        if (!ObjectUtils.isEmpty(reporterId) && !Objects.equals(reporterId, 0L)) {
                            userIds.add(reporterId);
                        }
                    });
                    Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
                    Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
                    Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
                    Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
                    Map<Long, List<SprintNameDTO>> closeSprintNames = issueMapper.querySprintNameByIssueIds(Arrays.asList(projectId), issueIds).stream().collect(Collectors.groupingBy(SprintNameDTO::getIssueId));
                    Map<Long, List<VersionIssueRelDTO>> fixVersionNames = issueMapper.queryVersionNameByIssueIds(Arrays.asList(projectId), issueIds, FIX_RELATION_TYPE).stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
                    Map<Long, List<VersionIssueRelDTO>> influenceVersionNames = issueMapper.queryVersionNameByIssueIds(Arrays.asList(projectId), issueIds, INFLUENCE_RELATION_TYPE).stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
                    Map<Long, List<LabelIssueRelDTO>> labelNames = issueMapper.queryLabelIssueByIssueIds(Arrays.asList(projectId), issueIds).stream().collect(Collectors.groupingBy(LabelIssueRelDTO::getIssueId));
                    Map<Long, List<ComponentIssueRelDTO>> componentMap = issueMapper.queryComponentIssueByIssueIds(Arrays.asList(projectId), issueIds).stream().collect(Collectors.groupingBy(ComponentIssueRelDTO::getIssueId));
                    Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectId, issueIds, true);
                    cursor
                            .addCollections(userIds)
                            .addCollections(usersMap)
                            .addCollections(issueTypeDTOMap)
                            .addCollections(statusMapDTOMap)
                            .addCollections(priorityDTOMap)
                            .addCollections(closeSprintNames)
                            .addCollections(fixVersionNames)
                            .addCollections(influenceVersionNames)
                            .addCollections(labelNames)
                            .addCollections(componentMap)
                            .addCollections(foundationCodeValue);
                    issues.forEach(issue -> {
                        Long issueId = issue.getIssueId();
                        ExportIssuesVO exportIssuesVO = new ExportIssuesVO();
                        BeanUtils.copyProperties(issue, exportIssuesVO);

                        exportIssuesVO.setProjectName(project.getName());
                        exportIssuesVO.setSprintName(getActiveSprintName(issue));
                        setAssignee(usersMap, issue, exportIssuesVO);
                        serReporter(usersMap, issue, exportIssuesVO);
                        setPriorityName(priorityDTOMap, issue, exportIssuesVO);
                        setStatusName(statusMapDTOMap, issue, exportIssuesVO);
                        setTypeName(issueTypeDTOMap, issue, exportIssuesVO);
                        setCloseSprintName(closeSprintNames, issueId, exportIssuesVO);
                        setFixVersionName(fixVersionNames, issueId, exportIssuesVO);
                        exportIssuesVO.setSprintName(exportIssuesSprintName(exportIssuesVO));
                        setInfluenceVersionName(influenceVersionNames, issueId, exportIssuesVO);
                        setLabelName(labelNames, issueId, exportIssuesVO);
                        setComponentName(componentMap, issueId, exportIssuesVO);
                        exportIssuesVO.setVersionName(exportIssuesVersionName(exportIssuesVO));
                        exportIssuesVO.setDescription(getDes(exportIssuesVO.getDescription()));
                        setFoundationFieldValue(foundationCodeValue, issueId, exportIssuesVO);
                        issueMap.put(issueId, exportIssuesVO);
                        processParentSonRelation(parentSonMap, issue);
                    });
                }
                ExcelUtil.writeIssue(issueMap, parentSonMap, ExportIssuesVO.class, fieldNames, fieldCodes, sheetName, Arrays.asList(AUTO_SIZE_WIDTH), workbook, cursor);
                boolean hasNextPage = cursor.getPage() < page.getTotalPages();
                cursor.clean();
                sendProcess(fileOperationHistoryDTO, userId, getProcess(cursor.getPage(), page.getTotalPages()));
                if (!hasNextPage) {
                    break;
                }
                //查询后页数增1
                cursor.increasePage();
            }
        }
//        ExcelUtil.writeToResponse(response, workbook);
        String fileName = project.getName() + FILESUFFIX;
        //把workbook上传到对象存储服务中
        downloadWorkBook(organizationId, workbook, fileName, fileOperationHistoryDTO, userId);
    }

    protected Double getProcess(Integer currentNum, Integer totalNum) {
        double process = (currentNum + 1.0) / (totalNum + 1.0) * 0.95 * 100;
        BigDecimal b = new BigDecimal(process);
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
        try (ByteArrayOutputStream os = new ByteArrayOutputStream();) {
            workbook.write(os);
            byte[] content = os.toByteArray();
            MultipartFile file = new MultipartExcel("file", fileName, EXCELCONTENTTYPE, content);

            //返回上载结果
            String path = fileClient.uploadFile(organizationId,APPLY_TYPE_AGILE,null, fileName, file);
            fileOperationHistoryDTO.setStatus(SUCCESS);
            fileOperationHistoryDTO.setFileUrl(path);
        } catch (Exception e) {
            fileOperationHistoryDTO.setStatus(FAILED);
        } finally {
            try {
                fileOperationHistoryDTO.setLastUpdateDate(new Date());
                fileOperationHistoryMapper.updateByPrimaryKey(fileOperationHistoryDTO);
                sendProcess(fileOperationHistoryDTO, userId, 100.0);

                //notifyService.postWebSocket(code, String.valueOf(userId), JSON.toJSONString(testFileLoadHistoryWithRateVO));
                workbook.close();
            } catch (IOException e) {
                log.warn(EXPORT_ERROR_WORKBOOK_CLOSE, e);
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
        result.add("问题类型");
        result.add("问题编号");
        result.add("概要");
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
        result.add("typeName");
        result.add("issueNum");
        result.add("summary");
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
        Set<Long> childrenSet =  parentSonMap.get(parentId);
        if (childrenSet == null) {
            childrenSet = new HashSet<>();
            parentSonMap.put(parentId, childrenSet);
        }
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

        if (exportFieldCodes != null && exportFieldCodes.size() != 0) {
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
        StringBuilder result = new StringBuilder();
        if (!"".equals(str) && str != null) {
            String[] arrayLine = str.split(("\\},\\{"));
            String regEx = "\"insert\":\"(.*)\"";
            Pattern pattern = Pattern.compile(regEx);
            for (String s : arrayLine) {
                Matcher matcher = pattern.matcher(s);
                if (matcher.find()) {
                    result.append(StringEscapeUtils.unescapeJava(matcher.group(1)));
                }
            }
        }
        return result.toString();
    }

    protected String exportIssuesSprintName(ExportIssuesVO exportIssuesVO) {
        StringBuilder sprintName = new StringBuilder(exportIssuesVO.getSprintName() != null ? "正在使用冲刺:" + exportIssuesVO.getSprintName() + "\r\n" : "");
        sprintName.append(!Objects.equals(exportIssuesVO.getCloseSprintName(), "") ? "已关闭冲刺:" + exportIssuesVO.getCloseSprintName() : "");
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
        order.put("issueId", "issue_issue_id");
        return PageableHelper.getSortSql(PageUtil.sortResetOrder(sort, null, order));
    }
}
