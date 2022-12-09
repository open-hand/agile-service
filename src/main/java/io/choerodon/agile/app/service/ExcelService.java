package io.choerodon.agile.app.service;

import java.io.InputStream;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.ServletRequestAttributes;

import io.choerodon.agile.api.vo.ExcelTemplateVO;
import io.choerodon.agile.api.vo.FileOperationHistoryVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.business.ExportIssuesVO;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.infra.dto.FileOperationHistoryDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.mybatis.pagehelper.domain.Sort;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
public interface ExcelService {

    void download(Long projectId, Long organizationId, HttpServletResponse response, ExcelTemplateVO excelTemplateVO);

    void batchImport(Long projectId, Long organizationId, Long userId, InputStream inputStream,  ServletRequestAttributes requestAttributes);

    void cancelImport(Long projectId, Long id, Long objectVersionNumber);

    FileOperationHistoryVO queryLatestRecode(Long projectId, String action);

    /**
     *
     * @param projectId
     * @param searchVO
     * @param request
     * @param response
     * @param organizationId
     * @param sort
     * @param requestAttributes
     * @see ExcelService#asyncExportIssuesV2(Long, SearchParamVO, HttpServletRequest, HttpServletResponse, Long, Sort, ServletRequestAttributes)
     */
    @Deprecated
    void asyncExportIssues(Long projectId, SearchVO searchVO, HttpServletRequest request,
                           HttpServletResponse response, Long organizationId, Sort sort, ServletRequestAttributes requestAttributes);

    void asyncExportIssuesV2(Long projectId,
                             SearchParamVO searchParamVO,
                             HttpServletRequest request,
                             HttpServletResponse response,
                             Long organizationId,
                             Sort sort,
                             ServletRequestAttributes requestAttributes);

    FileOperationHistoryDTO initFileOperationHistory(Long projectId, Long userId, String status, String action, String websocketKey);

    FileOperationHistoryDTO initFileOperationHistory(Long projectId, Long organizationId, Long userId, String status, String action, String websocketKey);

    void processExportField(List<String> exportFieldCodes,
                            String[] fieldsName,
                            String[] fields,
                            Map<String, String[]> fieldMap,
                            ObjectMapper objectMapper,
                            Object content);

    void downloadWorkBook(Long organizationId,Workbook workbook, String fileName, FileOperationHistoryDTO fileOperationHistoryDTO, Long userId);

    /**
     * ws发送进度
     * @param fileOperationHistoryDTO   文件历史DTO
     * @param userId                    操作用户ID
     * @param process                   进度, 请传入0~100之间的双精度浮点数!!!
     * @param websocketKey              websocketKey
     */
    void sendProcess(FileOperationHistoryDTO fileOperationHistoryDTO, Long userId, Double process, String websocketKey);

    /**
     * 查询组织层最后一次操作记录
     * @param organizationId 组织id
     * @param action 操作名称
     * @return 最后一次操作记录
     */
    FileOperationHistoryVO queryOrgLatestRecode(Long organizationId, String action);

    /**
     * 下载自定义字段导入模板
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param response response
     */
    void downloadObjectSchemeField(Long organizationId, Long projectId, HttpServletResponse response);

    /**
     * 导入自定义字段
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param inputStream excel文件流
     * @param requestAttributes 请求头
     */
    void batchImportObjectSchemeField(Long organizationId, Long projectId, InputStream inputStream, RequestAttributes requestAttributes);


    ExportIssuesVO buildExcelIssueFromIssue(String projectName,
                                            Map<Long, Set<Long>> parentSonMap,
                                            Map<Long, ExportIssuesVO> issueMap,
                                            Map<String, Object> issueValueMap,
                                            IssueDTO issue);
}
