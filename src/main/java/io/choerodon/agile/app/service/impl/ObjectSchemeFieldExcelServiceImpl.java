package io.choerodon.agile.app.service.impl;

import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.app.service.ObjectSchemeFieldExcelService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.infra.enums.CustomFieldExcelHeader;
import io.choerodon.agile.infra.enums.FieldTypeCnName;
import io.choerodon.agile.infra.utils.CatalogExcelUtil;
import io.choerodon.agile.infra.utils.ExcelUtil;
import io.choerodon.core.exception.CommonException;

/**
 * @author chihao.ran@hand-china.com
 * 2021/02/01 19:12
 */
@Service
public class ObjectSchemeFieldExcelServiceImpl implements ObjectSchemeFieldExcelService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ObjectSchemeFieldExcelServiceImpl.class);

    private static final String TEMPLATE_PATH = "templates";
    private static final String TEMPLATE_NAME = "ObjectSchemeFieldImportGuideTemplate.xlsx";
    private static final String IMPORT_TEMPLATE_NAME = "导入模板";

    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;

    @Override
    public void download(Long organizationId, Long projectId, HttpServletResponse response) {
        Workbook wb = new XSSFWorkbook();
        //复制模板excel
        copyGuideSheetFromTemplate(wb);
        Sheet sheet = wb.createSheet(IMPORT_TEMPLATE_NAME);
        CellStyle style = CatalogExcelUtil.getHeadStyle(wb);
        generateHeaders(sheet, style);
        //填充预定义值
        fillInPredefinedValues(wb, sheet, projectId, organizationId);
        try {
            wb.write(response.getOutputStream());
        } catch (Exception e) {
            LOGGER.info("exception: {}", e);
        }
    }

    private void copyGuideSheetFromTemplate(Workbook wb) {
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

    private void generateHeaders(Sheet sheet, CellStyle style) {
        CustomFieldExcelHeader[] headers = CustomFieldExcelHeader.values();
        Row row = sheet.createRow(0);
        for (int i = 0; i < headers.length; i++) {
            CustomFieldExcelHeader header = headers[i];
            sheet.setColumnWidth(i, header.getWidth());
            CatalogExcelUtil.initCell(row.createCell(i), style, header.getHeaderName());
        }
    }

    private void fillInPredefinedValues(Workbook wb, Sheet sheet, Long projectId, Long organizationId) {
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
}
