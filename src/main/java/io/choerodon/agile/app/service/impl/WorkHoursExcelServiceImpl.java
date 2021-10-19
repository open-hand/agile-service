package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.WorkHoursExcelService;
import io.choerodon.agile.app.service.WorkHoursService;
import io.choerodon.agile.infra.dto.ExcelCursorDTO;
import io.choerodon.agile.infra.dto.FileOperationHistoryDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.OrganizationInfoVO;
import io.choerodon.agile.infra.mapper.FileOperationHistoryMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.ExcelUtil;
import io.choerodon.agile.infra.utils.MultipartExcel;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.hzero.boot.file.FileClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-19 14:24
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkHoursExcelServiceImpl implements WorkHoursExcelService {
    protected static final Logger LOGGER = LoggerFactory.getLogger(WorkHoursExcelServiceImpl.class);
    private static final String EXPORT_WORK_HOURS_LOG = "agile-export-work-hours-log";
    private static final String EXCELCONTENTTYPE = "application/vnd.ms-excel";
    private static final String SUCCESS = "success";
    private static final String FAILED = "failed";
    private static final String DOING = "doing";
    private static final String DOWNLOAD_FILE = "download_file";
    private static final List<ExcelTitleVO> WORK_HOURS_LOG_LIST =  new ArrayList<>();
    private static final String EXPORT_ERROR_WORKBOOK_CLOSE = "error.close.workbook";

    @Autowired
    private WorkHoursService workHoursService;

    @Autowired
    private BaseFeignClient baseFeignClient;

    @Autowired
    private FileOperationHistoryMapper fileOperationHistoryMapper;

    @Autowired
    private FileClient fileClient;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private MessageClientC7n messageClientC7n;

    static {
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("登记人", "", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("耗费时间（单位：小时）", "workTime", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("工作日期", "workDate", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("工作项类型", "issueTypeName", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("工作项编号", "issueNum", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("工作项概要", "summary", 6000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("状态", "statusName", 4000));
        WORK_HOURS_LOG_LIST.add(new ExcelTitleVO("所属项目", "projectName", 4000));
    }

    @Override
    public void exportWorkHoursLog(Long organizationId,
                                   List<Long> projectIds,
                                   WorkHoursSearchVO workHoursSearchVO,
                                   ServletRequestAttributes requestAttributes,
                                   Boolean isOrg) {
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String websocketKey = buildWebSocketKey(EXPORT_WORK_HOURS_LOG, isOrg, organizationId, projectIds.get(0));
        String excelName = buildExcelName(organizationId,"工时日志", projectIds.get(0), isOrg);
        // 发送websocket
        Long projectId = 0L;
        if (Boolean.FALSE.equals(isOrg)) {
            projectId = projectIds.get(0);
        }
        FileOperationHistoryDTO fileOperationHistoryDTO = initFileOperationHistory(projectId, organizationId, userId, DOING, DOWNLOAD_FILE, websocketKey);
        // 构建excel标题栏
        Workbook workbook = ExcelUtil.initWorkHoursExportWorkbook("工时日志", WORK_HOURS_LOG_LIST);
        // 查询数据并写入excel
        ExcelCursorDTO cursor = new ExcelCursorDTO(1, 0, 1000);
        Sort.Order order = new Sort.Order(Sort.Direction.DESC, "creationDate");
        Sort sort = new Sort(order);
        while (true) {
            PageRequest pageRequest = new PageRequest(cursor.getPage(), cursor.getSize());
            pageRequest.setSort(sort);
            Page<WorkHoursLogVO> page = workHoursService.pageWorkHoursLogByProjectIds(organizationId, projectIds, pageRequest, workHoursSearchVO);
            if (CollectionUtils.isEmpty(page.getContent())) {
                break;
            }
            boolean hasNextPage = (cursor.getPage() + 1) < page.getTotalPages();
            cursor.clean();
            if (!hasNextPage) {
                break;
            }
        }
        // 上传excel到minio并发送websocket
        downLoadExcel(workbook, excelName, organizationId, websocketKey, userId, fileOperationHistoryDTO);
    }

    public FileOperationHistoryDTO initFileOperationHistory(Long projectId, Long organizationId, Long userId, String status, String action, String websocketKey) {
        FileOperationHistoryDTO fileOperationHistoryDTO = new FileOperationHistoryDTO(projectId, organizationId, userId, action, 0L, 0L, status);
        if (fileOperationHistoryMapper.insertSelective(fileOperationHistoryDTO) != 1) {
            throw new CommonException("error.FileOperationHistoryDTO.insert");
        }
        FileOperationHistoryDTO res = fileOperationHistoryMapper.selectByPrimaryKey(fileOperationHistoryDTO.getId());
        sendProcess(res, userId, 0.0, websocketKey);
        return res;
    }

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

    private void downLoadExcel(Workbook workbook,
                               String fileName,
                               Long organizationId,
                               String websocketKey,
                               Long userId,
                               FileOperationHistoryDTO fileOperationHistoryDTO) {
        try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
            workbook.write(os);
            byte[] content = os.toByteArray();
            MultipartFile file = new MultipartExcel("file", fileName, EXCELCONTENTTYPE, content);

            //返回上载结果
            String path = fileClient.uploadFile(organizationId,"agile",null, fileName, file);
            fileOperationHistoryDTO.setStatus(SUCCESS);
            fileOperationHistoryDTO.setFileUrl(path);
        } catch (Exception e) {
            fileOperationHistoryDTO.setStatus(FAILED);
        } finally {
            try {
                fileOperationHistoryDTO.setLastUpdateDate(new Date());
                fileOperationHistoryMapper.updateByPrimaryKey(fileOperationHistoryDTO);
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

    private String buildExcelName(Long organizationId, String functionName, Long projectId, Boolean isOrg) {
        if (isOrg) {
            OrganizationInfoVO organizationInfoVO = baseFeignClient.query(organizationId).getBody();
            return organizationInfoVO.getTenantName() + "-" + functionName;
        } else {
            ProjectVO projectVO = ConvertUtil.queryProject(projectId);
            return projectVO.getName() + "-" + functionName;
        }
    }

    private String buildWebSocketKey(String exportWorkHoursLog, Boolean isOrg, Long organizationId, Long projectId) {
        if (isOrg) {
            return exportWorkHoursLog + "-org-" + organizationId;
        } else {
            return exportWorkHoursLog + "-" + projectId;
        }
    }
}
