package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.PredefinedDTO;
import io.choerodon.agile.infra.enums.ExcelImportTemplate;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.List;
import java.util.Map;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/10
 */
public interface ExcelCommonService {

    PredefinedDTO processLabelPredefined(Long projectId,
                                         ExcelImportTemplate.Cursor cursor,
                                         List<String> fieldCodes);

    PredefinedDTO processEpicOrFeaturePredefined(Long organizationId,
                                                 Long projectId,
                                                 boolean withFeature,
                                                 ExcelImportTemplate.Cursor cursor,
                                                 List<String> fieldCodes);

    Map<String, Long> getEpicMap(Long projectId);

    PredefinedDTO buildPredefinedByFieldCodeAndValues(ExcelImportTemplate.Cursor cursor,
                                                      List<String> fieldCodes,
                                                      List<String> values,
                                                      String fieldCode);

    PredefinedDTO processSprintPredefined(Long projectId,
                                          ExcelImportTemplate.Cursor cursor,
                                          List<String> fieldCodes);

    PredefinedDTO processComponentPredefined(Long projectId,
                                             ExcelImportTemplate.Cursor cursor,
                                             List<String> fieldCodes);

    PredefinedDTO processVersionPredefined(Long projectId,
                                           ExcelImportTemplate.Cursor cursor,
                                           List<String> fieldCodes);

    PredefinedDTO processInfluenceVersionPredefined(Long projectId,
                                                    ExcelImportTemplate.Cursor cursor,
                                                    List<String> fieldCodes);

    PredefinedDTO processPriorityPredefined(Long organizationId,
                                            ExcelImportTemplate.Cursor cursor,
                                            List<String> fieldCodes);

    PredefinedDTO processIssueStatusPredefined(Long organizationId,
                                               Long projectId,
                                               ExcelImportTemplate.Cursor cursor,
                                               List<String> fieldCodes);

    PredefinedDTO processIssueProductPredefined(Long organizationId, Long projectId, ExcelImportTemplate.Cursor cursor, List<String> fieldCodes);

    int getColByFieldCode(List<String> fieldCodes, String fieldCode);

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
}
