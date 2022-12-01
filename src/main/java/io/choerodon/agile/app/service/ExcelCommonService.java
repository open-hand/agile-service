package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.alibaba.fastjson.JSONObject;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import io.choerodon.agile.api.vo.ExcelColumnVO;
import io.choerodon.agile.api.vo.PageFieldViewUpdateVO;
import io.choerodon.agile.api.vo.business.IssueExcelImportVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.domain.entity.ExcelSheetData;
import io.choerodon.agile.infra.dto.FileOperationHistoryDTO;
import io.choerodon.agile.infra.dto.PredefinedDTO;
import io.choerodon.agile.infra.enums.ExcelImportTemplate;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/10
 */
public interface ExcelCommonService {

    PredefinedDTO processSystemFieldPredefined(Long projectId,
                                               ExcelImportTemplate.Cursor cursor,
                                               boolean withFeature,
                                               List<String> fieldCodes,
                                               String fieldCode);

    Map<String, Long> getEpicMap(Long projectId);

    int getColByFieldCode(List<String> fieldCodes, String fieldCode);

    /**
     * 计算进度, (currentNum + 1 / totalNum + 1) * 0.95 * 100
     * @param currentNum    currentNum
     * @param totalNum      totalNum
     * @return              百分百表示的进度
     */
    Double getProcess(Integer currentNum, Integer totalNum);

    void copyGuideSheetFromTemplate(Workbook wb, String path);

    List<PredefinedDTO> processCustomFieldPredefinedList(Long projectId,
                                                         List<String> customFields,
                                                         ExcelImportTemplate.Cursor cursor,
                                                         int systemFieldLength,
                                                         Map<String, String> customFieldCodeNameMap,
                                                         String issueTypeList);

    void fillInPredefinedValues(Workbook wb, Sheet sheet, List<PredefinedDTO> predefinedList);

    Map<String, Long> getManagers(Long projectId);

    void validateWorkbook(Workbook workbook,
                          FileOperationHistoryDTO history,
                          String websocketKey,
                          String templateSheetName);

    List<String> resolveCodeFromHeader(Workbook workbook,
                                       FileOperationHistoryDTO history,
                                       String websocketKey);

    void addSystemFieldIfDateType(String code,
                                  int col,
                                  ExcelColumnVO excelColumnVO);

    void addSystemFieldIfDateType(Set<Integer> dateTypeColumns,
                                  String code,
                                  int col,
                                  ExcelColumnVO excelColumnVO);

//    void validateCustomField(Map<Integer, ExcelColumnVO> headerMap,
//                             Long projectId,
//                             FileOperationHistoryDTO history,
//                             String issueTypeList,
//                             String websocketKey);

    void validateCustomField(Map<Integer, ExcelColumnVO> headerMap,
                             Long projectId,
                             FileOperationHistoryDTO history,
                             String issueTypeList,
                             Set<Integer> dateTypeColumns,
                             String websocketKey);

    int processErrorData(Long userId,
                         FileOperationHistoryDTO history,
                         JSONObject sheetData,
                         Integer dataRowCount,
                         ExcelImportTemplate.Progress progress,
                         int rowNum,
                         Set<Integer> sonSet,
                         int parentColIndex, int lastSendCountNum,
                         String websocketKey);

    int processErrorData(Long userId,
                         FileOperationHistoryDTO history,
                         Sheet dataSheet,
                         Integer dataRowCount,
                         ExcelImportTemplate.Progress progress,
                         Map<Integer, List<Integer>> errorRowColMap,
                         int rowNum,
                         Set<Integer> sonSet,
                         int parentColIndex, int lastSendCountNum,
                         String websocketKey);

    void insertCustomFields(Long issueId,
                            List<PageFieldViewUpdateVO> customFields,
                            Long projectId);

    void generateErrorDataExcelAndUpload(Map<Integer, List<Integer>> errorRowColMap,
                                         Sheet dataSheet,
                                         Map<Integer, ExcelColumnVO> headerMap,
                                         List<String> headerNames,
                                         FileOperationHistoryDTO history,
                                         Long organizationId,
                                         String templatePath);

    void updateFinalRecode(FileOperationHistoryDTO fileOperationHistoryDTO,
                           Long successCount,
                           Long failCount,
                           String status,
                           String websocketKey);

    void setCommonSystemFieldPredefinedValueByCode(String code,
                                                   Long projectId,
                                                   Long organizationId,
                                                   ExcelColumnVO excelColumnVO,
                                                   boolean withFeature);

    void validateCustomFieldData(JSONObject rowJson,
                                 Integer col,
                                 ExcelColumnVO excelColumn,
                                 IssueExcelImportVO issueExcelImportVO);

    void validateCustomFieldData(Row row,
                                 Integer col,
                                 ExcelColumnVO excelColumn,
                                 Map<Integer, List<Integer>> errorRowColMap,
                                 IssueExcelImportVO issueExcelImportVO);

    void handlerRequireFiled(ExcelColumnVO excelColumn,
                             Map<Long, List<String>> requireFieldMap,
                             IssueExcelImportVO issueExcelImportVO,
                             Long projectId);

    Boolean checkRequireField(Map<Long, List<String>> requireFieldMap,
                              ExcelColumnVO excelColumn,
                              IssueExcelImportVO issueExcelImportVO,
                              JSONObject rowJson,
                              Integer col);

    Boolean checkRequireField(Map<Long, List<String>> requireFieldMap,
                              ExcelColumnVO excelColum,
                              IssueExcelImportVO issueExcelImportVO,
                              Row row,
                              Integer col,
                              Map<Integer, List<Integer>> errorRowColMap);

    void validateCommonSystemFieldData(JSONObject rowJson,
                                       Integer col,
                                       ExcelColumnVO excelColumn,
                                       IssueExcelImportVO issueExcelImportVO,
                                       IssueVO parentIssue,
                                       Long projectId,
                                       Map<Integer, ExcelColumnVO> headerMap);

    void validateCommonSystemFieldData(Row row,
                                       Integer col,
                                       ExcelColumnVO excelColumn,
                                       Map<Integer, List<Integer>> errorRowColMap,
                                       IssueExcelImportVO issueExcelImportVO,
                                       IssueVO parentIssue,
                                       Long projectId,
                                       Map<Integer, ExcelColumnVO> headerMap);

    void putErrorMsg(JSONObject rowJson,
                     JSONObject cellJson,
                     String errorMsg);

    void setErrorMsgToParentSonRow(int rowNum,
                                   JSONObject sheetData,
                                   Set<Integer> sonSet,
                                   int parentColIndex);

    String generateErrorDataExcelAndUpload(ExcelSheetData excelSheetData,
                                           Map<Integer, ExcelColumnVO> headerMap,
                                           List<String> headerNames,
                                           FileOperationHistoryDTO history,
                                           Long organizationId,
                                           String templatePath);

    void fieldCascadeValidate(Long projectId,
                              IssueExcelImportVO issueExcelImportVO,
                              Map<Integer, ExcelColumnVO> headerMap,
                              JSONObject rowJson);
}
