package io.choerodon.agile.app.service.impl;

import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.hzero.boot.file.FileClient;
import org.hzero.core.base.BaseConstants;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.FieldOptionUpdateVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.ObjectSchemeFieldCreateVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.app.service.FieldOptionService;
import io.choerodon.agile.app.service.ObjectSchemeFieldExcelService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.FileOperationHistoryDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.enums.CustomFieldExcelHeader;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.enums.FieldTypeCnName;
import io.choerodon.agile.infra.mapper.FileOperationHistoryMapper;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldExtendMapper;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;
import io.choerodon.agile.infra.utils.CatalogExcelUtil;
import io.choerodon.agile.infra.utils.ExcelUtil;
import io.choerodon.agile.infra.utils.MultipartExcelUtil;
import io.choerodon.core.exception.CommonException;

/**
 * @author chihao.ran@hand-china.com
 * 2021/02/01 19:12
 */
@Service
public class ObjectSchemeFieldExcelServiceImpl implements ObjectSchemeFieldExcelService {

    private static final int HEADER_LENGTH = 8;
    private static final int NOT_KEY_HEADER_LENGTH = 5;

    private static final String PRO = "pro_";
    private static final String ORG = "org_";

    protected static final String BACKETNAME = "agile-service";
    protected static final String MULTIPART_NAME = "file";
    protected static final String ORIGINAL_FILE_NAME = ".xlsx";
    protected static final String FILE_NAME = "error.xlsx";


    private static final String AGILE_ISSUE = "agile_issue";
    private static final String TEMPLATE_PATH = "templates";
    private static final String TEMPLATE_NAME = "ObjectSchemeFieldImportGuideTemplate.xlsx";

    private static final String CANCELED = "canceled";
    private static final String UPLOAD_FILE_CUSTOM_FIELD = "upload_file_customer_field";

    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    protected FieldOptionService fieldOptionService;
    @Autowired
    private FileOperationHistoryMapper fileOperationHistoryMapper;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired
    private UserService userService;
    @Autowired
    private FileClient fileClient;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public void generateErrorDataExcelAndUpload(Map<Integer, List<Integer>> errorRowColMap, Workbook workbook, Sheet sheet, FileOperationHistoryDTO history, Long organizationId) {
        CellStyle ztStyle = workbook.createCellStyle();
        Font ztFont = workbook.createFont();
        ztFont.setColor(Font.COLOR_RED);
        ztStyle.setFont(ztFont);
        for (Map.Entry<Integer, List<Integer>> entry : errorRowColMap.entrySet()) {
            int rowNum = entry.getKey();
            List<Integer> errorCols = entry.getValue();
            Row row = sheet.getRow(rowNum);
            if (CollectionUtils.isEmpty(errorCols)) {
                continue;
            }
            errorCols.forEach(errorColNum -> {
                Cell cell = row.getCell(errorColNum);
                if (!isCellEmpty(cell)) {
                    cell.setCellStyle(ztStyle);
                }
            });
        }
        MultipartFile multipartFile = new MultipartExcelUtil(MULTIPART_NAME, ORIGINAL_FILE_NAME, workbook);
        String errorWorkBookUrl = fileClient.uploadFile(organizationId, BACKETNAME, null, FILE_NAME, multipartFile);
        history.setFileUrl(errorWorkBookUrl);
    }

    @Override
    public void createObjectSchemeField(Long projectId, Long organizationId, ObjectSchemeFieldCreateVO objectSchemeFieldCreate, List<IssueTypeVO> issueTypes) {
        ObjectSchemeFieldDTO field = modelMapper.map(objectSchemeFieldCreate, ObjectSchemeFieldDTO.class);
        Set<Long> issueTypeIds = new HashSet<>(objectSchemeFieldCreate.getIssueTypeIds());
        List<IssueTypeVO> setIssueTypes = issueTypes.stream().filter(issueTypeVO -> issueTypeIds.contains(issueTypeVO.getId())).collect(Collectors.toList());
        Set<String> typeCodes = setIssueTypes.stream().map(IssueTypeVO::getTypeCode).collect(Collectors.toSet());

        field.setContext(String.join(",", typeCodes));
        field.setOrganizationId(organizationId);
        field.setProjectId(projectId);
        field = objectSchemeFieldService.baseCreate(field, setIssueTypes, null);

        //处理字段选项
        if (objectSchemeFieldCreate.getFieldOptions() != null) {
            String defaultIds = fieldOptionService.handleFieldOption(organizationId, field.getId(), objectSchemeFieldCreate.getFieldOptions());
            if (defaultIds != null && !"".equals(defaultIds)) {
                field.setDefaultValue(defaultIds);
                objectSchemeFieldMapper.updateOptional(field, "defaultValue");
                objectSchemeFieldExtendMapper.selectExtendFieldByOptions(null, organizationId, field.getId(), projectId).forEach(f -> {
                    f.setDefaultValue(defaultIds);
                    objectSchemeFieldExtendMapper.updateByPrimaryKey(f);
                });
            }
        }
    }

    @Override
    public Map<String, UserVO> getUserNameMap(Long organizationId, Long projectId) {
        List<UserVO> userList;
        if (projectId != null) {
            userList = userService.listAllUsersByProject(projectId);
        } else {
            userList = userService.listAllUsersByOrganization(organizationId);
        }
        if (CollectionUtils.isEmpty(userList)) {
            return new HashMap<>(0);
        }
        return userList.stream().collect(Collectors.toMap(UserVO::getRealName, a -> a, (k1, k2) -> k1));
    }

    @Override
    public void validAndSetDefaultValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, Map<String, Integer> keyRowMap, Row row, Map<String, UserVO> userNameMap, Map<Integer, List<Integer>> errorRowColMap) {
        String fieldType = objectSchemeFieldCreate.getFieldType();

        Cell cell = row.getCell(4);
        if (isCellEmpty(cell)) {
            objectSchemeFieldCreate.setDefaultValue(null);
            return;
        }
        if (StringUtils.isEmpty(fieldType)) {
            return;
        }
        switch (fieldType) {
            case FieldType.RADIO:
            case FieldType.CHECKBOX:
            case FieldType.SINGLE:
            case FieldType.MULTIPLE:
                cell.setCellType(CellType.STRING);
                objectSchemeFieldCreate.setDefaultValue(cell.toString());
                validateOptionDefaultValue(fieldType, objectSchemeFieldCreate.getFieldOptions(), objectSchemeFieldCreate.getDefaultValue(), keyRowMap, row, errorRowColMap);
                break;
            case FieldType.MEMBER:
            case FieldType.MULTI_MEMBER:
                cell.setCellType(CellType.STRING);
                validateMemberDefaultValue(objectSchemeFieldCreate, cell.toString(), row, userNameMap, errorRowColMap);
                break;
            case FieldType.NUMBER:
                cell.setCellType(CellType.STRING);
                objectSchemeFieldCreate.setDefaultValue(cell.toString());
                validateNumberDefaultValue(objectSchemeFieldCreate.getDefaultValue(), row, errorRowColMap);
                break;
            case FieldType.DATETIME:
            case FieldType.TIME:
            case FieldType.DATE:
                validateDateDefaultValue(objectSchemeFieldCreate, row, cell, errorRowColMap);
                break;
            case FieldType.TEXT:
            case FieldType.INPUT:
                cell.setCellType(CellType.STRING);
                objectSchemeFieldCreate.setDefaultValue(cell.toString());
                break;
            default:
                break;
        }
    }

    private void validateMemberDefaultValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, String defaultValue, Row row, Map<String, UserVO> userNameMap, Map<Integer, List<Integer>> errorRowColMap) {
        List<String> memberNames = splitByComma(defaultValue);
        List<Long> ids = new ArrayList<>();
        StringBuilder errMsg = new StringBuilder();
        memberNames.forEach(memberName -> {
            if (userNameMap.get(memberName) == null) {
                errMsg.append("\"").append(memberName).append("\", ");
            } else {
                ids.add(userNameMap.get(memberName).getId());
            }
        });
        if (!StringUtils.isEmpty(errMsg.toString())) {
            errMsg.append("不存在");
            row.getCell(4).setCellValue(buildWithErrorMsg(defaultValue, errMsg.toString()));
            addErrorColumn(row.getRowNum(), 4, errorRowColMap);
        } else {
            objectSchemeFieldCreate.setDefaultValue(ids.stream().map(String::valueOf).collect(Collectors.joining(",")));
        }
    }

    private void validateDateDefaultValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, Row row, Cell cell, Map<Integer, List<Integer>> errorRowColMap) {
        SimpleDateFormat sdf = new SimpleDateFormat(BaseConstants.Pattern.DATETIME);
        String defaultValue = objectSchemeFieldCreate.getDefaultValue();
        if (CellType.STRING.equals(cell.getCellTypeEnum()) && "当前时间".equals(defaultValue)) {
            objectSchemeFieldCreate.setExtraConfig(true);
            objectSchemeFieldCreate.setDefaultValue(sdf.format(new Date()));
            return;
        }
        if (!CellType.NUMERIC.equals(cell.getCellTypeEnum()) || !DateUtil.isCellDateFormatted(cell)) {
            cell.setCellType(CellType.STRING);
            cell.setCellValue(buildWithErrorMsg(cell.toString(), "请输入格式正确的日期"));
            addErrorColumn(row.getRowNum(), 4, errorRowColMap);
            return;
        }
        objectSchemeFieldCreate.setDefaultValue(sdf.format(cell.getDateCellValue()));
    }

    private void validateNumberDefaultValue(String defaultValue, Row row, Map<Integer, List<Integer>> errorRowColMap) {
        if (!Pattern.matches("^[0-9]*$", defaultValue)) {
            row.getCell(4).setCellValue(buildWithErrorMsg(defaultValue, "当前字段类型默认值只能为数字"));
            addErrorColumn(row.getRowNum(), 4, errorRowColMap);
        }
    }

    private void validateOptionDefaultValue(String fieldType, List<FieldOptionUpdateVO> fieldOptions, String defaultValue, Map<String, Integer> keyRowMap, Row row, Map<Integer, List<Integer>> errorRowColMap) {

        Map<String, FieldOptionUpdateVO> optionMap;
        List<String> defaultKeys;

        if (!CollectionUtils.isEmpty(fieldOptions)) {
            optionMap = fieldOptions.stream().collect(Collectors.toMap(
                    FieldOptionUpdateVO::getCode, option -> option, (oldVal, currVal) -> oldVal));
        } else {
            optionMap = new HashMap<>(0);
        }

        switch (fieldType) {
            case FieldType.RADIO:
            case FieldType.SINGLE:
                defaultKeys = Collections.singletonList(defaultValue);
                break;
            case FieldType.CHECKBOX:
            case FieldType.MULTIPLE:
                defaultKeys = splitByComma(defaultValue);
                break;
            default:
                defaultKeys = new ArrayList<>();
                break;
        }

        StringBuilder errMsg = new StringBuilder();

        defaultKeys.forEach(defaultKey -> {
            if (optionMap.get(defaultKey) != null){
                optionMap.get(defaultKey).setDefault(true);
            }
            if (keyRowMap.get(defaultKey) == null) {
                errMsg.append("\"").append(defaultKey).append("\", ");
            }
        });
        if (!StringUtils.isEmpty(errMsg.toString())) {
            errMsg.append("不在值有效范围内");
            row.getCell(4).setCellValue(buildWithErrorMsg(defaultValue, errMsg.toString()));
            addErrorColumn(row.getRowNum(), 4, errorRowColMap);
        }
    }

    @Override
    public Map<String, Integer> validKeyValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, Sheet sheet, int r, Map<Integer, List<Integer>> errorRowColMap) {
        Map<String, Integer> keyRowMap = new HashMap<>(objectSchemeFieldCreate.getFieldOptions().size());
        if (!FieldTypeCnName.isOption(objectSchemeFieldCreate.getFieldType())) {
            setNotOptionError(objectSchemeFieldCreate, sheet, r, errorRowColMap);
        } else {
            validIsOptionKeyValue(objectSchemeFieldCreate, sheet, r, keyRowMap, errorRowColMap);
        }
        return keyRowMap;
    }

    private void validIsOptionKeyValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, Sheet sheet, int r, Map<String, Integer> keyRowMap, Map<Integer, List<Integer>> errorRowColMap) {
        int jumpRow = isKeyValue(sheet.getRow(r)) ? 0 : 1;
        if (CollectionUtils.isEmpty(objectSchemeFieldCreate.getFieldOptions())) {
            Row row = sheet.getRow(r);
            row.createCell(5).setCellValue(buildWithErrorMsg("", "字段列表不能为空"));
            addErrorColumn(row.getRowNum(), 5, errorRowColMap);
            addErrorColumn(r, 10, errorRowColMap);
            return;
        }
        for (int i = 0; i < objectSchemeFieldCreate.getFieldOptions().size(); i++) {
            FieldOptionUpdateVO fieldOption = objectSchemeFieldCreate.getFieldOptions().get(i);
            Row row = sheet.getRow(r + jumpRow + i);

            if (StringUtils.isEmpty(fieldOption.getCode())) {
                row.createCell(5).setCellValue(buildWithErrorMsg("", "值不能为空"));
                addErrorColumn(row.getRowNum(), 5, errorRowColMap);
                addErrorColumn(r, 10, errorRowColMap);
            } else if (keyRowMap.get(fieldOption.getCode()) != null) {
                row.getCell(5).setCellValue(buildWithErrorMsg(fieldOption.getCode(), "值重复"));
                addErrorColumn(row.getRowNum(), 5, errorRowColMap);
                Row firstRepeatRow = sheet.getRow(keyRowMap.get(fieldOption.getCode()));
                firstRepeatRow.getCell(5).setCellValue(buildWithErrorMsg(fieldOption.getCode(), "值重复"));
                addErrorColumn(firstRepeatRow.getRowNum(), 5, errorRowColMap);
                addErrorColumn(r, 10, errorRowColMap);
            } else {
                keyRowMap.put(fieldOption.getCode(), row.getRowNum());
            }
            if (StringUtils.isEmpty(fieldOption.getCode())) {
                row.createCell(6).setCellValue(buildWithErrorMsg("", "显示值不能为空"));
                addErrorColumn(row.getRowNum(), 6, errorRowColMap);
                addErrorColumn(r, 10, errorRowColMap);
            }
            if (fieldOption.getEnabled() == null) {
                row.createCell(7).setCellValue(buildWithErrorMsg("", "是否启用输入错误"));
                addErrorColumn(row.getRowNum(), 7, errorRowColMap);
                addErrorColumn(r, 10, errorRowColMap);
            }
        }
    }

    private void setNotOptionError(ObjectSchemeFieldCreateVO objectSchemeFieldCreate, Sheet sheet, int r, Map<Integer, List<Integer>> errorRowColMap) {
        int jumpRow = isKeyValue(sheet.getRow(r)) ? 0 : 1;
        for (int i = 0; i < objectSchemeFieldCreate.getFieldOptions().size(); i++) {
            Row row = sheet.getRow(r + jumpRow + i);
            row.createCell(5).setCellValue(buildWithErrorMsg("", "当前问题类型无法设置值列表"));
            addErrorColumn(row.getRowNum(), 5, errorRowColMap);
        }
    }

    @Override
    public ObjectSchemeFieldCreateVO generateObjectSchemeField(Long organizationId, Long projectId, Row row, Map<Integer, List<Integer>> errorRowColMap, Map<String, IssueTypeVO> issueTypeNameMap) {
        ObjectSchemeFieldCreateVO objectSchemeFieldCreateVO = new ObjectSchemeFieldCreateVO();
        objectSchemeFieldCreateVO.setFieldOptions(new ArrayList<>());
        objectSchemeFieldCreateVO.setSchemeCode(AGILE_ISSUE);
        objectSchemeFieldCreateVO.setExtraConfig(false);
        validateAndSetCode(organizationId, projectId, objectSchemeFieldCreateVO, row, errorRowColMap);
        validateAndSetName(organizationId, projectId, objectSchemeFieldCreateVO, row, errorRowColMap);
        validateAndSetFieldType(objectSchemeFieldCreateVO, row, errorRowColMap);
        validateAndSetIssueType(objectSchemeFieldCreateVO, row, errorRowColMap, issueTypeNameMap);
        if (isKeyValue(row)) {
            setKeyValue(objectSchemeFieldCreateVO, row);
        }
        return objectSchemeFieldCreateVO;
    }

    @Override
    public void setKeyValue(ObjectSchemeFieldCreateVO objectSchemeFieldCreateVO, Row row) {
        List<FieldOptionUpdateVO> optionList = objectSchemeFieldCreateVO.getFieldOptions();
        FieldOptionUpdateVO fieldOptionUpdateVO = new FieldOptionUpdateVO();
        fieldOptionUpdateVO.setStatus("add");
        Cell keyCell = row.getCell(5);
        Cell valueCell = row.getCell(6);
        Cell enabledCell = row.getCell(7);
        if (!isCellEmpty(keyCell)) {
            keyCell.setCellType(CellType.STRING);
            String key = keyCell.toString();
            fieldOptionUpdateVO.setCode(key);
        }
        if (!isCellEmpty(valueCell)) {
            valueCell.setCellType(CellType.STRING);
            String value = valueCell.toString();
            fieldOptionUpdateVO.setValue(value);
        }
        if (isCellEmpty(enabledCell)) {
            fieldOptionUpdateVO.setEnabled(false);
        } else {
            enabledCell.setCellType(CellType.STRING);
            String enabled = enabledCell.toString();
            if ("是".equals(enabled)) {
                fieldOptionUpdateVO.setEnabled(true);
            } else if ("否".equals(enabled)) {
                fieldOptionUpdateVO.setEnabled(false);
            }
        }
        optionList.add(fieldOptionUpdateVO);
    }

    private void validateAndSetIssueType(ObjectSchemeFieldCreateVO objectSchemeFieldCreateVO, Row row, Map<Integer, List<Integer>> errorRowColMap, Map<String, IssueTypeVO> issueTypeNameMap) {
        Cell cell = row.getCell(3);
        if (isCellEmpty(cell)) {
            row.createCell(3).setCellValue(buildWithErrorMsg("", "问题类型不能为空"));
            addErrorColumn(row.getRowNum(), 3, errorRowColMap);
            return;
        }
        cell.setCellType(CellType.STRING);
        String issueType = cell.toString();
        StringBuilder errMsg = new StringBuilder();
        Set<Long> issueTypeIds = getIssueTypeIdsByCnName(issueType, issueTypeNameMap, errMsg);
        if (!StringUtils.isEmpty(errMsg.toString())) {
            cell.setCellValue(buildWithErrorMsg(issueType, errMsg.toString()));
            addErrorColumn(row.getRowNum(), 3, errorRowColMap);
        } else {
            objectSchemeFieldCreateVO.setIssueTypeIds(new ArrayList<>(issueTypeIds));
        }
    }

    private Set<Long> getIssueTypeIdsByCnName(String issueType, Map<String, IssueTypeVO> issueTypeNameMap, StringBuilder errMsg) {
        Set<Long> issueTypeIds = new HashSet<>();
        List<String> issueTypeStrList = splitByComma(issueType);
        issueTypeStrList.forEach(issueTypeStr -> {
            if (issueTypeNameMap.get(issueTypeStr) == null) {
                errMsg.append("\"").append(issueTypeStr).append("\", ");
            } else {
                issueTypeIds.add(issueTypeNameMap.get(issueTypeStr).getId());
            }
        });
        if (CollectionUtils.isEmpty(issueTypeIds) || !StringUtils.isEmpty(errMsg.toString())) {
            errMsg.append("问题类型错误");
        }
        return issueTypeIds;
    }

    private void validateAndSetFieldType(ObjectSchemeFieldCreateVO objectSchemeFieldCreateVO, Row row, Map<Integer, List<Integer>> errorRowColMap) {
        Cell cell = row.getCell(2);
        if (isCellEmpty(cell)) {
            row.createCell(2).setCellValue(buildWithErrorMsg("", "字段类型不能为空"));
            addErrorColumn(row.getRowNum(), 2, errorRowColMap);
            return;
        }
        cell.setCellType(CellType.STRING);
        String fieldType = cell.toString();
        Optional<FieldTypeCnName> fieldTypeCnNameOptional = FieldTypeCnName.getFieldTypeByCnName(fieldType);
        if (!fieldTypeCnNameOptional.isPresent()) {
            cell.setCellValue(buildWithErrorMsg(fieldType, "字段类型错误"));
            addErrorColumn(row.getRowNum(), 2, errorRowColMap);
        } else {
            objectSchemeFieldCreateVO.setFieldType(fieldTypeCnNameOptional.get().getCode());
        }
    }

    private void validateAndSetName(Long organizationId, Long projectId, ObjectSchemeFieldCreateVO objectSchemeFieldCreateVO, Row row, Map<Integer, List<Integer>> errorRowColMap) {
        Cell cell = row.getCell(1);
        if (isCellEmpty(cell)) {
            row.createCell(1).setCellValue(buildWithErrorMsg("", "名称不能为空"));
            addErrorColumn(row.getRowNum(), 1, errorRowColMap);
            return;
        }
        cell.setCellType(CellType.STRING);
        String name = cell.toString();
        if (Boolean.TRUE.equals(objectSchemeFieldService.checkName(organizationId, projectId, name, AGILE_ISSUE))) {
            cell.setCellValue(buildWithErrorMsg(name, "名称已经存在"));
            addErrorColumn(row.getRowNum(), 1, errorRowColMap);
        } else if (name.length() > 30) {
            cell.setCellValue(buildWithErrorMsg(name, "名称字符数需要在30个以内"));
            addErrorColumn(row.getRowNum(), 1, errorRowColMap);
        } else {
            objectSchemeFieldCreateVO.setName(name);
        }
    }

    private void validateAndSetCode(Long organizationId, Long projectId, ObjectSchemeFieldCreateVO objectSchemeFieldCreateVO, Row row, Map<Integer, List<Integer>> errorRowColMap) {
        Cell cell = row.getCell(0);
        if (isCellEmpty(cell)) {
            row.createCell(0).setCellValue(buildWithErrorMsg("", "编码不能为空"));
            addErrorColumn(row.getRowNum(), 0, errorRowColMap);
            return;
        }
        cell.setCellType(CellType.STRING);
        String code = (projectId == null ? ORG : PRO) + cell.toString();
        if (Boolean.TRUE.equals(objectSchemeFieldService.checkCode(organizationId, projectId, code, AGILE_ISSUE))) {
            cell.setCellValue(buildWithErrorMsg(code, "编码已经存在"));
            addErrorColumn(row.getRowNum(), 0, errorRowColMap);
        } else if (code.length() > 30) {
            cell.setCellValue(buildWithErrorMsg(code, "编码字符数需要在30个以内"));
            addErrorColumn(row.getRowNum(), 0, errorRowColMap);
        } else if (!Pattern.matches("^[0-9a-zA-Z_]+$", code)) {
            cell.setCellValue(buildWithErrorMsg(code, "编码只允许数字、字母及下划线组成"));
            addErrorColumn(row.getRowNum(), 0, errorRowColMap);
        } else {
            objectSchemeFieldCreateVO.setCode(code);
        }
    }

    @Override
    public boolean isSkip(Row row) {
        if (row == null) {
            return true;
        }
        //所有列都为空才跳过
        boolean skip = true;
        for (int i = 0; i < HEADER_LENGTH; i++) {
            Cell cell = row.getCell(i);
            skip = skip && isCellEmpty(cell);
        }
        return skip;
    }

    @Override
    public boolean isKeyValue(Row row) {
        if (row == null) {
            return false;
        }
        //后3个字段任有一个有值视为其为keyValue行
        for (int i = NOT_KEY_HEADER_LENGTH; i < HEADER_LENGTH; i++) {
            Cell cell = row.getCell(i);
            if (!isCellEmpty(cell)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean isExtendKeyValue(Row row) {
        if (row == null) {
            return false;
        }
        //前面5个字段任有一个有值视为非keyValue行
        for (int i = 0; i < NOT_KEY_HEADER_LENGTH; i++) {
            Cell cell = row.getCell(i);
            if (!isCellEmpty(cell)) {
                return false;
            }
        }
        return isKeyValue(row);
    }

    @Override
    public boolean validExcelTemplate(Workbook workbook, FileOperationHistoryDTO history) {
        boolean result;
        if (workbook.getNumberOfSheets() < 2) {
            result = false;
        } else {
            Sheet dataSheet = workbook.getSheetAt(1);
            Row headerRow = dataSheet.getRow(0);
            result = (headerRow != null);
        }
        if (!result) {
            history.setStatus("empty_data_sheet");
            fileOperationHistoryMapper.updateByPrimaryKeySelective(history);
        }
        return result;
    }

    @Override
    public void copyGuideSheetFromTemplate(Workbook wb) {
        Sheet guideSheet = wb.createSheet("要求");
        InputStream inputStream = this.getClass().getResourceAsStream("/" + TEMPLATE_PATH + "/" + TEMPLATE_NAME);
        XSSFWorkbook srcWorkbook;
        try {
            srcWorkbook = new XSSFWorkbook(inputStream);
        } catch (IOException e) {
            throw new CommonException("error.open.custom.field.guide.template");
        }
        ExcelUtil.copySheet(srcWorkbook.getSheetAt(0), guideSheet, ExcelUtil.copyCellStyle(srcWorkbook, wb));
    }

    @Override
    public void generateHeaders(Sheet sheet, CellStyle style) {
        CustomFieldExcelHeader[] headers = CustomFieldExcelHeader.values();
        Row row = sheet.createRow(0);
        for (int i = 0; i < headers.length; i++) {
            CustomFieldExcelHeader header = headers[i];
            sheet.setColumnWidth(i, header.getWidth());
            CatalogExcelUtil.initCell(row.createCell(i), style, header.getHeaderName());
        }
    }

    @Override
    public void fillInPredefinedValues(Workbook wb, Sheet sheet, Long projectId, Long organizationId) {
        ExcelUtil.dropDownList2007(wb,
                sheet,
                getFieldTypePredefined(),
                1, 500,
                2, 2,
                "fieldType", 2);
        ExcelUtil.dropDownList2007(wb,
                sheet,
                getIssueTypePredefined(projectId, organizationId),
                1, 500,
                3, 3,
                "issueType", 3);
        ExcelUtil.dropDownList2007(wb,
                sheet,
                Arrays.asList("是", "否"),
                1, 500,
                7, 7,
                "enabled", 4);
    }

    private List<String> getIssueTypePredefined(Long projectId, Long organizationId) {
        List<IssueTypeVO> issueTypeList = objectSchemeFieldService.issueTypes(organizationId, projectId);
        return issueTypeList.stream().map(IssueTypeVO::getName).collect(Collectors.toList());
    }

    private List<String> getFieldTypePredefined() {
        return Arrays.stream(FieldTypeCnName.values()).map(FieldTypeCnName::getName).collect(Collectors.toList());
    }

    private boolean isCellEmpty(Cell cell) {
        return cell == null || "".equals(cell.toString()) || cell.getCellTypeEnum() == CellType.BLANK;
    }

    @Override
    public boolean checkCanceled(Long organizationId, Long projectId, Long fileOperationHistoryId, List<Long> importedFieldIds) {
        FileOperationHistoryDTO checkCanceledDO = fileOperationHistoryMapper.selectByPrimaryKey(fileOperationHistoryId);
        if (UPLOAD_FILE_CUSTOM_FIELD.equals(checkCanceledDO.getAction())
                && CANCELED.equals(checkCanceledDO.getStatus())) {
            if (!importedFieldIds.isEmpty()) {
                importedFieldIds.forEach(importedFieldId ->
                        objectSchemeFieldService.delete(organizationId, projectId, importedFieldId));
            }
            return true;
        }
        return false;
    }

    private String buildWithErrorMsg(String value, String msg) {
        return value + "(" + msg + ")";
    }

    private void addErrorColumn(int rowNum, Integer col, Map<Integer, List<Integer>> errorRowColMap) {
        List<Integer> columns = errorRowColMap.computeIfAbsent(rowNum, key -> new ArrayList<>());
        columns.add(col);
    }

    private List<String> splitByComma(String value) {
        String regex = "[,]\\s*|[，]\\s*";
        return Arrays.asList(value.split(regex));
    }

    @Override
    public Integer getRealRowCount(Sheet sheet) {
        Integer count = 0;
        for (int r = 1; r <= sheet.getPhysicalNumberOfRows(); r++) {
            Row row = sheet.getRow(r);
            //row为空跳过
            if (isSkip(row) || (r > 1 && !isSkip(sheet.getRow(r - 1)) && isExtendKeyValue(row))) {
                continue;
            }
            count++;
        }
        return count;
    }
}
