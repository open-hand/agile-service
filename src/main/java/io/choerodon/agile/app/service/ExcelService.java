package io.choerodon.agile.app.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.FileOperationHistoryVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.dto.FileOperationHistoryDTO;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.Map;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
public interface ExcelService {

    void download(Long projectId, Long organizationId, HttpServletRequest request, HttpServletResponse response);

    void batchImport(Long projectId, Long organizationId, Long userId, Workbook workbook);

    void cancelImport(Long projectId, Long id, Long objectVersionNumber);

    FileOperationHistoryVO queryLatestRecode(Long projectId, String action);

    void asyncExportIssues(Long projectId, SearchVO searchVO, HttpServletRequest request,
                           HttpServletResponse response, Long organizationId, Sort sort, ServletRequestAttributes requestAttributes);

    FileOperationHistoryDTO initFileOperationHistory(Long projectId, Long userId, String status, String action);

    void processExportField(List<String> exportFieldCodes,
                            String[] fieldsName,
                            String[] fields,
                            Map<String, String[]> fieldMap,
                            ObjectMapper objectMapper,
                            Object content);

    void downloadWorkBook(Long organizationId,Workbook workbook, String fileName, FileOperationHistoryDTO fileOperationHistoryDTO, Long userId);

    void sendProcess(FileOperationHistoryDTO fileOperationHistoryDTO, Long userId, Double process);
}
