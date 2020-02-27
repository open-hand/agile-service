package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.github.pagehelper.PageInfo;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.domain.IssueType;
import io.choerodon.agile.app.domain.Predefined;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.FileFeignClient;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.DefaultTransactionDefinition;
import org.springframework.util.ObjectUtils;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.math.BigDecimal;
import java.util.*;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class ExcelServiceImpl implements ExcelService {

    protected static final Logger LOGGER = LoggerFactory.getLogger(ExcelServiceImpl.class);

    protected static final String[] FIELDS_NAME =
            {"问题类型*", "所属史诗", "模块", "冲刺", "概述*", "子任务概述(仅子任务生效)", "经办人",
                    "优先级", "预估时间(小时)", "版本", "史诗名称(仅问题类型为史诗时生效)", "故事点"};
    protected static final String[] FIELDS =
            {"typeName", "belongsEpic", "component", "sprint", "summary", "subTaskSummary",
                    "manager", "priorityName", "remainTime", "version", "epicName", "storyPoints"};

    //    protected static final String[] FIELDS_NAME = {"概要", "描述", "优先级", "问题类型", "故事点", "剩余时间", "修复版本", "史诗名称", "模块", "冲刺"};
//
//
//    protected static final String[] FIELDS = {"summary", "description", "priorityName", "typeName", "storyPoints", "remainTime", "version", "epicName", "component", "sprint"};
    protected static final String BACKETNAME = "agile-service";
    protected static final String SUB_TASK = "sub_task";
    protected static final String UPLOAD_FILE = "upload_file";
    protected static final String APPLY_TYPE_AGILE = "agile";
    protected static final String CANCELED = "canceled";
    protected static final String DOING = "doing";
    protected static final String SUCCESS = "success";
    protected static final String FAILED = "failed";
    protected static final String WEBSOCKET_IMPORT_CODE = "agile-import-issues";
    protected static final String STORY = "story";
    protected static final String ISSUE_EPIC = "issue_epic";
    protected static final String FEATURE = "feature";
    protected static final String FILE_NAME = "error.xlsx";
    protected static final String MULTIPART_NAME = "file";
    protected static final String ORIGINAL_FILE_NAME = ".xlsx";
    protected static final String VERSION_PLANNING = "version_planning";
    protected static final String HIDDEN_PRIORITY = "hidden_priority";
    protected static final String HIDDEN_ISSUE_TYPE = "hidden_issue_type";
    protected static final String HIDDEN_FIX_VERSION = "hidden_fix_version";
    protected static final String HIDDEN_COMPONENT = "hidden_component";
    protected static final String HIDDEN_SPRINT = "hidden_sprint";
    protected static final String RELATION_TYPE_FIX = "fix";
    protected static final String IMPORT_TEMPLATE_NAME = "导入模板";

    @Autowired
    protected StateMachineClientService stateMachineClientService;

    @Autowired
    private NotifyFeignClient notifyFeignClient;

    @Autowired
    private FileOperationHistoryMapper fileOperationHistoryMapper;

    @Autowired
    private FileFeignClient fileFeignClient;

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
    protected PlatformTransactionManager transactionManager;

    @Autowired
    protected BaseFeignClient baseFeignClient;

    private ModelMapper modelMapper = new ModelMapper();

    @PostConstruct
    public void init() {
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
    }

    @Override
    public void download(Long projectId, Long organizationId, HttpServletRequest request, HttpServletResponse response) {
        List<Predefined> predefinedList = getPredefinedList(organizationId, projectId);
        //所属史诗预定义值
        predefinedList.add(getEpicPredefined(projectId));

        Workbook wb = new XSSFWorkbook();
        // create guide sheet
        ExcelUtil.createGuideSheet(wb, ExcelUtil.initGuideSheet());
        Sheet sheet = wb.createSheet(IMPORT_TEMPLATE_NAME);
        Row row = sheet.createRow(0);
        CellStyle style = CatalogExcelUtil.getHeadStyle(wb);
        int columnNum = FIELDS_NAME.length;
        for (int i = 0; i < columnNum; i++) {
            int width = 3500;
            //子任务名称和史诗名称两列加宽
            if (i == 5 || i == 10) {
                width = 8000;
            }
            sheet.setColumnWidth(i, width);
        }
        generateHeaders(row, style, Arrays.asList(FIELDS_NAME));

        try {
            //填充预定义值
            fillInPredefinedValues(wb, sheet, predefinedList);
            wb.write(response.getOutputStream());
        } catch (Exception e) {
            LOGGER.info(e.getMessage());
        }
    }

    private Predefined getEpicPredefined(Long projectId) {
        List<EpicDataVO> epics = issueService.listEpic(projectId);
        List<String> values = new ArrayList<>();
        epics.forEach(e -> {
            Long id = e.getIssueId();
            String summary = e.getSummary();
            values.add(id + ":" + summary);
        });
        return new Predefined(values, 1, 500, 1, 1, "hidden_epic", 8);
    }

    protected void fillInPredefinedValues(Workbook wb, Sheet sheet, List<Predefined> predefinedList) {
        for (Predefined predefined : predefinedList) {
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

    protected List<Predefined> getPredefinedList(Long organizationId, Long projectId) {
        List<Predefined> predefinedList = new ArrayList<>();
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
        predefinedList.add(new Predefined(priorityList, 1, 500, 7, 7, HIDDEN_PRIORITY, 2));

        List<String> issueTypeList = new ArrayList<>();
        for (IssueTypeVO issueTypeVO : issueTypeVOList) {
            if (!SUB_TASK.equals(issueTypeVO.getTypeCode()) && !FEATURE.equals(issueTypeVO.getTypeCode())) {
                issueTypeList.add(issueTypeVO.getName());
            }
        }
        predefinedList.add(new Predefined(issueTypeList, 1, 500, 0, 0, HIDDEN_ISSUE_TYPE, 3));

        List<String> versionList = new ArrayList<>();
        for (ProductVersionCommonDTO productVersionCommonDTO : productVersionCommonDTOList) {
            if (VERSION_PLANNING.equals(productVersionCommonDTO.getStatusCode())) {
                versionList.add(productVersionCommonDTO.getName());
            }
        }
        predefinedList.add(new Predefined(versionList, 1, 500, 9, 9, HIDDEN_FIX_VERSION, 4));

        List<String> componentList = new ArrayList<>();
        for (IssueComponentDTO issueComponentDTO : issueComponentDTOList) {
            componentList.add(issueComponentDTO.getName());
        }
        predefinedList.add(new Predefined(componentList, 1, 500, 2, 2, HIDDEN_COMPONENT, 5));

        List<String> sprintList = new ArrayList<>();
        for (SprintDTO sprintDTO : sprintDTOList) {
            sprintList.add(sprintDTO.getSprintName());
        }
        predefinedList.add(new Predefined(sprintList, 1, 500, 3, 3, HIDDEN_SPRINT, 6));

        ResponseEntity<PageInfo<UserDTO>> response = baseFeignClient.listUsersByProjectId(projectId, 1, 0);
        List<UserDTO> users = response.getBody().getList();
        List<String> managers = new ArrayList<>();
        users.forEach(u -> {
            String realName = u.getRealName();
            String loginName = u.getLoginName();
            managers.add(realName + "(" + loginName + ")");
        });
        predefinedList.add(new Predefined(managers, 1, 500, 6, 6, "hidden_manager", 7));
        return predefinedList;
    }

    protected void generateHeaders(Row row, CellStyle style, List<String> headers) {
        for (int i = 0; i < headers.size(); i++) {
            CatalogExcelUtil.initCell(row.createCell(i), style, headers.get(i));
        }
    }

    protected Boolean setIssueCreateInfo(IssueCreateVO issueCreateVO, Long projectId, Row row, Map<String, IssueTypeVO> issueTypeMap, Map<String, Long> priorityMap, Map<String, Long> versionMap, Long userId, Map<String, Long> componentMap, Map<String, Long> sprintMap) {
        String summary = row.getCell(0).toString();
        if (summary == null) {
            throw new CommonException("error.summary.null");
        }
        String description = null;
        if (!(row.getCell(1) == null || row.getCell(1).toString().equals("") || row.getCell(1).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            description = row.getCell(1).toString();
        }
        String priorityName = row.getCell(2).toString();
        String typeName = row.getCell(3).toString();
        if (priorityMap.get(priorityName) == null) {
            return false;
        }
        if (issueTypeMap.get(typeName) == null) {
            return false;
        }
        BigDecimal storyPoint = null;
        if (!(row.getCell(4) == null || row.getCell(4).toString().equals("") || row.getCell(4).getCellType() == XSSFCell.CELL_TYPE_BLANK) && "故事".equals(typeName)) {
            storyPoint = new BigDecimal(row.getCell(4).toString());
        }
        BigDecimal remainTime = null;
        if (!(row.getCell(5) == null || row.getCell(5).toString().equals("") || row.getCell(5).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            remainTime = new BigDecimal(row.getCell(5).toString());
        }
        String versionName = null;
        if (!(row.getCell(6) == null || row.getCell(6).toString().equals("") || row.getCell(6).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            versionName = row.getCell(6).toString();
        }
        String epicName = null;
        if (!(row.getCell(7) == null || row.getCell(7).toString().equals("") || row.getCell(7).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            epicName = row.getCell(7).toString();
        }
        String componentName = null;
        if (!(row.getCell(8) == null || row.getCell(8).toString().equals("") || row.getCell(8).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            componentName = row.getCell(8).toString();
        }
        String sprintName = null;
        if (!(row.getCell(9) == null || row.getCell(9).toString().equals("") || row.getCell(9).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            sprintName = row.getCell(9).toString();
        }
        List<VersionIssueRelVO> versionIssueRelVOList = null;
        if (!(versionName == null || "".equals(versionName))) {
            versionIssueRelVOList = new ArrayList<>();
            VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
            versionIssueRelVO.setVersionId(versionMap.get(versionName));
            versionIssueRelVO.setRelationType(RELATION_TYPE_FIX);
            versionIssueRelVOList.add(versionIssueRelVO);
        }
        String typeCode = issueTypeMap.get(typeName).getTypeCode();
        issueCreateVO.setProjectId(projectId);
        issueCreateVO.setSummary(summary);
        if (description != null) {
            issueCreateVO.setDescription("[{\"insert\":\"" + StringUtil.replaceChar(description) + "\\n\"}]");
        }
        issueCreateVO.setPriorityCode("priority" + priorityMap.get(priorityName));
        issueCreateVO.setPriorityId(priorityMap.get(priorityName));
        issueCreateVO.setIssueTypeId(issueTypeMap.get(typeName).getId());
        issueCreateVO.setTypeCode(typeCode);
        // 当问题类型为故事，设置故事点
        if (STORY.equals(typeCode)) {
            issueCreateVO.setStoryPoints(storyPoint);
        }
        // 当问题类型为史诗，默认史诗名称与概要相同
        if (ISSUE_EPIC.equals(typeCode)) {
            issueCreateVO.setEpicName(summary);
            issueCreateVO.setEpicName(epicName);
        }
        List<ComponentIssueRelVO> componentIssueRelVOList = null;
        if (!(componentName == null || "".equals(componentName))) {
            componentIssueRelVOList = new ArrayList<>();
            ComponentIssueRelVO componentIssueRelVO = new ComponentIssueRelVO();
            componentIssueRelVO.setComponentId(componentMap.get(componentName));
            componentIssueRelVOList.add(componentIssueRelVO);
        }
        if (sprintName != null) {
            issueCreateVO.setSprintId(sprintMap.get(sprintName));
        }
        issueCreateVO.setComponentIssueRelVOList(componentIssueRelVOList);
        issueCreateVO.setRemainingTime(remainTime);
        issueCreateVO.setVersionIssueRelVOList(versionIssueRelVOList);
        issueCreateVO.setReporterId(userId);
        return true;
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

    protected void setIssueTypeAndPriorityMap(Long organizationId, Long projectId, Map<String, IssueTypeVO> issueTypeMap, Map<String, Long> priorityMap, List<String> issueTypeList, List<String> priorityList) {
        List<PriorityVO> priorityVOList = priorityService.queryByOrganizationIdList(organizationId);
        List<IssueTypeVO> issueTypeVOList = projectConfigService.queryIssueTypesByProjectId(projectId, APPLY_TYPE_AGILE);
        for (PriorityVO priorityVO : priorityVOList) {
            if (priorityVO.getEnable()) {
                priorityMap.put(priorityVO.getName(), priorityVO.getId());
                priorityList.add(priorityVO.getName());
            }
        }
        for (IssueTypeVO issueTypeVO : issueTypeVOList) {
            if (!SUB_TASK.equals(issueTypeVO.getTypeCode()) && !FEATURE.equals(issueTypeVO.getTypeCode())) {
                issueTypeMap.put(issueTypeVO.getName(), issueTypeVO);
            }
        }
        //添加子任务/子缺陷
        issueTypeMap.put("子任务", issueTypeMap.get("任务"));
        issueTypeMap.put("子缺陷", issueTypeMap.get("缺陷"));
        issueTypeList.addAll(issueTypeMap.keySet());
    }

    protected void sendProcess(FileOperationHistoryDTO fileOperationHistoryDTO, Long userId, Double process) {
        fileOperationHistoryDTO.setProcess(process);
        notifyFeignClient.postWebSocket(WEBSOCKET_IMPORT_CODE, userId.toString(), JSON.toJSONString(fileOperationHistoryDTO));
    }

    protected String uploadErrorExcel(Workbook errorWorkbook) {
        // 上传错误的excel
        ResponseEntity<String> response = fileFeignClient.uploadFile(BACKETNAME, FILE_NAME, new MultipartExcelUtil(MULTIPART_NAME, ORIGINAL_FILE_NAME, errorWorkbook));
        if (response == null || response.getStatusCode() != HttpStatus.OK) {
            throw new CommonException("error.errorWorkbook.upload");
        }
        return response.getBody();
    }

    protected Boolean checkEpicNameExist(Long projectId, String epicName) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setEpicName(epicName);
        List<IssueDTO> issueDTOList = issueMapper.select(issueDTO);
        return issueDTOList == null || issueDTOList.isEmpty();
    }

    protected Map<Integer, String> checkRule(Long projectId, Sheet sheet, List<String> issueTypeList,
                                             List<String> priorityList, List<String> versionList,
                                             Map<String, IssueTypeVO> issueTypeMap, List<String> componentList,
                                             List<String> sprintList, int rowNum, Set<Integer> illegalRow) {
        Row row = sheet.getRow(rowNum);
        Map<Integer, String> errorMessage = new HashMap<>();
        // check summary
        if (row.getCell(0) == null || row.getCell(0).toString().equals("") || row.getCell(0).getCellType() == XSSFCell.CELL_TYPE_BLANK) {
            errorMessage.put(0, "概要不能为空");
        } else if (row.getCell(0).toString().length() > 44) {
            errorMessage.put(0, "概要过长");
        }
        // check priority
        if (row.getCell(2) == null || row.getCell(2).toString().equals("") || row.getCell(2).getCellType() == XSSFCell.CELL_TYPE_BLANK) {
            errorMessage.put(2, "优先级不能为空");
        } else if (!priorityList.contains(row.getCell(2).toString())) {
            errorMessage.put(2, "优先级输入错误");
        }
        // check issue type
        if (illegalRow.contains(rowNum)) {
            errorMessage.put(3, "子任务/子缺陷必须有父节点");
        } else if (row.getCell(3) == null || row.getCell(3).toString().equals("") || row.getCell(3).getCellType() == XSSFCell.CELL_TYPE_BLANK) {
            errorMessage.put(3, "问题类型不能为空");
        } else if (!issueTypeList.contains(row.getCell(3).toString())) {
            errorMessage.put(3, "问题类型输入错误");
        }
        // check story point
        if (!(row.getCell(4) == null || row.getCell(4).toString().equals("") || row.getCell(4).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            String storyPointStr = row.getCell(4).toString().trim();
            if (storyPointStr.length() > 3) {
                errorMessage.put(4, "请输入正确的位数");
            } else if (!NumberUtil.isNumeric(storyPointStr)) {
                errorMessage.put(4, "请输入数字");
            } else {
                if (NumberUtil.isInteger(storyPointStr) || NumberUtil.canParseInteger(storyPointStr)) {
                    if (storyPointStr.trim().length() > 3) {
                        errorMessage.put(4, "最大支持3位整数");
                    } else if (storyPointStr.trim().length() > 1 && "0".equals(storyPointStr.trim().substring(0, 0))) {
                        errorMessage.put(4, "请输入正确的整数");
                    }
                } else if (!"0.5".equals(storyPointStr)) {
                    errorMessage.put(4, "小数只支持0.5");
                }
            }
        }
        // check remain time
        if (!(row.getCell(5) == null || row.getCell(5).toString().equals("") || row.getCell(5).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            String remainTime = row.getCell(5).toString().trim();
            if (remainTime.length() > 3) {
                errorMessage.put(5, "请输入正确的位数");
            } else if (!NumberUtil.isNumeric(remainTime)) {
                errorMessage.put(5, "请输入数字");
            } else {
                if (NumberUtil.isInteger(remainTime) || NumberUtil.canParseInteger(remainTime)) {
                    if (remainTime.trim().length() > 3) {
                        errorMessage.put(5, "最大支持3位整数");
                    } else if (remainTime.trim().length() > 1 && "0".equals(remainTime.trim().substring(0, 0))) {
                        errorMessage.put(5, "请输入正确的整数");
                    }
                } else if (!"0.5".equals(remainTime)) {
                    errorMessage.put(5, "小数只支持0.5");
                }
            }
        }
        // check version
        if (!(row.getCell(6) == null || row.getCell(6).toString().equals("") || row.getCell(6).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            if (!versionList.contains(row.getCell(6).toString())) {
                errorMessage.put(6, "请输入正确的版本");
            }
        }
        // check epic name
        if (!(row.getCell(3) == null || row.getCell(3).toString().equals("") || row.getCell(3).getCellType() == XSSFCell.CELL_TYPE_BLANK)
                && issueTypeList.contains(row.getCell(3).toString())
                && ISSUE_EPIC.equals(issueTypeMap.get(row.getCell(3).toString()).getTypeCode())) {
            if (row.getCell(7) == null || row.getCell(7).toString().equals("") || row.getCell(7).getCellType() == XSSFCell.CELL_TYPE_BLANK) {
                errorMessage.put(7, "史诗名称不能为空");
            } else {
                String epicName = row.getCell(7).toString().trim();
                if (epicName.length() > 10) {
                    errorMessage.put(7, "史诗名称过长");
                } else if (!checkEpicNameExist(projectId, epicName)) {
                    errorMessage.put(7, "史诗名称重复");
                }
            }
        }
        // check component
        if (!(row.getCell(8) == null || row.getCell(8).toString().equals("") || row.getCell(8).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            if (!componentList.contains(row.getCell(8).toString())) {
                errorMessage.put(8, "请输入正确的模块");
            }
        }
        // check sprint
        if (!(row.getCell(9) == null || row.getCell(9).toString().equals("") || row.getCell(9).getCellType() == XSSFCell.CELL_TYPE_BLANK)) {
            if (!sprintList.contains(row.getCell(9).toString())) {
                errorMessage.put(9, "请输入正确的冲刺");
            }
        }
        return errorMessage;
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
        FileOperationHistoryDTO res = initFileOperationHistory(projectId, userId, status);
        validateWorkbook(projectId, userId, workbook, res);

        Sheet sheet = workbook.getSheetAt(1);
        // 获取所有非空行
        int columnNum = 10;
        Integer allRowCount = getRealRowCount(sheet, columnNum);
        // 查询组织下的优先级与问题类型
        Map<String, IssueTypeVO> issueTypeMap = new HashMap<>();
        Map<String, Long> priorityMap = new HashMap<>();
        List<String> issueTypeList = new ArrayList<>();
        List<String> priorityList = new ArrayList<>();
        setIssueTypeAndPriorityMap(organizationId, projectId, issueTypeMap, priorityMap, issueTypeList, priorityList);
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
        List<Long> importedIssueIds = new ArrayList<>();

        Map<Integer, String> allIssueType = new LinkedHashMap<>();
        List<IssueType> issueTypes = getAllIssueType(allRowCount, sheet, columnNum, allIssueType);
        Map<Integer, Set<Integer>> parentSonMap = getParentSonMap(issueTypes);
        Map<Integer, Integer> sonParentMap = getSonParentMap(parentSonMap);
        //获取无父节点的子任务/子缺陷
        Set<Integer> illegalRow = getIllegalRow(allIssueType, sonParentMap);

        for (int r = 1; r <= allRowCount; r++) {
            if (checkCanceled(projectId, res.getId(), importedIssueIds)) {
                return;
            }
            Row row = sheet.getRow(r);
            if (isSkip(row, columnNum)) {
                continue;
            }
            for (int w = 0; w < FIELDS.length; w++) {
                if (row.getCell(w) != null) {
                    row.getCell(w).setCellType(XSSFCell.CELL_TYPE_STRING);
                }
            }
            String typeName = row.getCell(3).toString();
            //有子节点的故事和任务，要和子节点一块校验，有一个不合法，则全为错误的
            Set<Integer> set = parentSonMap.get(r);
            Boolean hasSonNodes = (set != null && !set.isEmpty());
            if (("故事".equals(typeName) || "任务".equals(typeName)) && hasSonNodes) {
                Map<String, Object> returnMap = batchCheck(projectId, sheet, issueTypeList, priorityList,
                        versionList, issueTypeMap, componentList, sprintList, r, illegalRow, set, columnNum);
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

                Set<Long> insertIds = batchInsert(projectId, row, issueTypeMap, priorityMap, versionMap, userId, componentMap, sprintMap, sheet, set);
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
                        versionList, issueTypeMap, componentList, sprintList, r, illegalRow);
                if (!errorMap.isEmpty()) {
                    failCount++;
                    processErrorMap(errorMapList, r, row, errorMap, errorRows);
                    res.setFailCount(failCount);
                    processNum++;
                    sendProcess(res, userId, processNum * 1.0 / allRowCount);
                    continue;
                }
                IssueCreateVO issueCreateVO = new IssueCreateVO();
                Boolean ok = setIssueCreateInfo(issueCreateVO, projectId, row, issueTypeMap, priorityMap, versionMap, userId, componentMap, sprintMap);

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
            Workbook result = ExcelUtil.generateExcelAwesome(workbook, errorRows, errorMapList, FIELDS_NAME, priorityList, issueTypeList, versionList, IMPORT_TEMPLATE_NAME, componentList, sprintList);
            String errorWorkBookUrl = uploadErrorExcel(result);
            res.setFileUrl(errorWorkBookUrl);
            status = FAILED;
        } else {
            status = SUCCESS;
        }
        updateFinalRecode(res, successCount, failCount, status);
    }

    protected Set<Long> batchInsert(Long projectId, Row row, Map<String, IssueTypeVO> issueTypeMap,
                                    Map<String, Long> priorityMap, Map<String, Long> versionMap,
                                    Long userId, Map<String, Long> componentMap, Map<String, Long> sprintMap,
                                    Sheet sheet, Set<Integer> set) {
        Set<Long> issueIds = new HashSet<>();
        //批量插入
        DefaultTransactionDefinition def = new DefaultTransactionDefinition();
        // explicitly setting the transaction name is something that can only be done programmatically
        def.setName("batchCommit");
        def.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRED);
        TransactionStatus txStatus = transactionManager.getTransaction(def);
        //插入父节点
        IssueCreateVO issueCreateVO = new IssueCreateVO();
        Boolean ok = setIssueCreateInfo(issueCreateVO, projectId, row, issueTypeMap, priorityMap, versionMap, userId, componentMap, sprintMap);
        IssueVO parent = null;
        if (ok) {
            parent = stateMachineClientService.createIssue(issueCreateVO, APPLY_TYPE_AGILE);
        }
        if (parent == null) {
            //回滚
            transactionManager.rollback(txStatus);
            return issueIds;
        }
        Long parentId = parent.getIssueId();
        issueIds.add(parentId);
        //处理子节点
        set.forEach(s -> {
            Row r = sheet.getRow(s);
            IssueCreateVO issueCreate = new IssueCreateVO();
            Boolean success = setIssueCreateInfo(issueCreate, projectId, r, issueTypeMap, priorityMap, versionMap, userId, componentMap, sprintMap);
            if (success) {
                String typeCode = issueCreate.getTypeCode();
                if (issueTypeMap.get("任务").getTypeCode().equals(typeCode)) {
                    issueCreate.setParentIssueId(parentId);
                }
                if (issueTypeMap.get("缺陷").getTypeCode().equals(typeCode)) {
                    issueCreate.setRelateIssueId(parentId);
                }
                IssueVO result = stateMachineClientService.createIssue(issueCreate, APPLY_TYPE_AGILE);
                if (result != null) {
                    issueIds.add(result.getIssueId());
                }
            }
        });
        if (set.size() + 1 == issueIds.size()) {
            transactionManager.commit(txStatus);
            return issueIds;
        } else {
            //有失败的数据，所有的都标记为失败，回滚数据
            transactionManager.rollback(txStatus);
            return new HashSet<>();
        }
    }

    protected Map<String, Object> batchCheck(Long projectId, Sheet sheet, List<String> issueTypeList,
                                             List<String> priorityList, List<String> versionList, Map<String, IssueTypeVO> issueTypeMap,
                                             List<String> componentList, List<String> sprintList, int rowNum,
                                             Set<Integer> illegalRow, Set<Integer> sonSet, int columnNum) {
        //key为row,value为错误信息
        Map<Integer, Map<Integer, String>> map = new HashMap<>();
        //key为列，value为错误详情
        Map<Integer, String> errorMap = checkRule(projectId, sheet, issueTypeList, priorityList,
                versionList, issueTypeMap, componentList, sprintList, rowNum, illegalRow);
        if (!errorMap.isEmpty()) {
            map.put(rowNum, errorMap);
        }
        Set<Integer> newSet = new HashSet<>();
        sonSet.forEach(r -> {
            Row row = sheet.getRow(r);
            if (isSkip(row, columnNum)) {
                return;
            }
            for (int w = 0; w < FIELDS.length; w++) {
                if (row.getCell(w) != null) {
                    row.getCell(w).setCellType(XSSFCell.CELL_TYPE_STRING);
                }
            }
            newSet.add(r);
            Map<Integer, String> error = checkRule(projectId, sheet, issueTypeList, priorityList,
                    versionList, issueTypeMap, componentList, sprintList, r, illegalRow);
            if (!error.isEmpty()) {
                map.put(r, error);
            }
        });
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
        }
        error.put(3, "父子结构中有错误数据");
    }

    protected Set<Integer> getIllegalRow(Map<Integer, String> allIssueType, Map<Integer, Integer> sonParentMap) {
        Set<Integer> set = new HashSet<>();
        for (Map.Entry<Integer, String> entry : allIssueType.entrySet()) {
            Integer key = entry.getKey();
            String value = entry.getValue();
            if ("子任务".equals(value) || "子缺陷".equals(value)) {
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

    protected Map<Integer, Set<Integer>> getParentSonMap(List<IssueType> issueTypes) {
        Map<Integer, Set<Integer>> map = new HashMap<>();
        for (IssueType issueType : issueTypes) {
            Integer row = issueType.getRow();
            String type = issueType.getType();
            //故事下只有子缺陷和子任务
            if ("故事".equals(type)) {
                storyRecursive(map, issueType, row);
            }
            //任务下只能有子任务
            if ("任务".equals(type)) {
                taskRecursive(map, issueType, row);
            }
        }
        return map;
    }

    private void taskRecursive(Map<Integer, Set<Integer>> map, IssueType issueType, Integer row) {
        if (issueType.hasNext()) {
            IssueType next = issueType.getNext();
            String nextType = next.getType();
            Integer nextRow = next.getRow();
            if ("子任务".equals(nextType)) {
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

    private void storyRecursive(Map<Integer, Set<Integer>> map, IssueType issueType,
                                Integer row) {
        if (issueType.hasNext()) {
            IssueType next = issueType.getNext();
            String nextType = next.getType();
            Integer nextRow = next.getRow();
            if ("子缺陷".equals(nextType) || "子任务".equals(nextType)) {
                processSonRow(map, row, nextRow);
                storyRecursive(map, next, row);
            }
        }
    }

    protected List<IssueType> getAllIssueType(Integer allRowCount, Sheet sheet, int columnNum, Map<Integer, String> allIssueType) {
        List<IssueType> issueTypes = new ArrayList<>();
        for (int i = 1; i <= allRowCount; i++) {
            int size = issueTypes.size();
            IssueType lastIssueType = null;
            if (size > 0) {
                lastIssueType = issueTypes.get(size - 1);
            }
            Row row = sheet.getRow(i);
            if (isSkip(row, columnNum)) {
                continue;
            }
            String type = row.getCell(3).toString();
            IssueType issueType = new IssueType(i, type);
            issueTypes.add(issueType);
            if (lastIssueType != null) {
                lastIssueType.setNext(issueType);
            }
            allIssueType.put(i, type);
        }
        return issueTypes;
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

    protected void validateWorkbook(Long projectId, Long userId, Workbook workbook, FileOperationHistoryDTO res) {
        if (workbook.getActiveSheetIndex() < 1
                || workbook.getSheetAt(1) == null
                || workbook.getSheetAt(1).getSheetName() == null
                || !IMPORT_TEMPLATE_NAME.equals(workbook.getSheetAt(1).getSheetName())) {
            if (fileOperationHistoryMapper.updateByPrimaryKeySelective(new FileOperationHistoryDTO(projectId, res.getId(), UPLOAD_FILE, "template_error", res.getObjectVersionNumber())) != 1) {
                throw new CommonException("error.FileOperationHistoryDTO.update");
            }
            FileOperationHistoryDTO errorImport = fileOperationHistoryMapper.selectByPrimaryKey(res.getId());
            sendProcess(errorImport, userId, 0.0);
            throw new CommonException("error.sheet.import");
        }
    }

    protected FileOperationHistoryDTO initFileOperationHistory(Long projectId, Long userId, String status) {
        FileOperationHistoryDTO fileOperationHistoryDTO = new FileOperationHistoryDTO(projectId, userId, UPLOAD_FILE, 0L, 0L, status);
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
    public FileOperationHistoryVO queryLatestRecode(Long projectId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        FileOperationHistoryDTO result = fileOperationHistoryMapper.queryLatestRecode(projectId, userId);
        return result == null ? new FileOperationHistoryVO() : modelMapper.map(result, FileOperationHistoryVO.class);
    }
}
