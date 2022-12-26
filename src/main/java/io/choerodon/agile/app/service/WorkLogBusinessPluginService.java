package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.WorkLogVO;
import io.choerodon.agile.infra.dto.WorkLogDTO;

/**
 * 工时日志商业插件
 * @author gaokuo.dai@zknow.com 2022-12-26
 */
public interface WorkLogBusinessPluginService {


    void checkBeforeCreateWorkLog(Long organizationId, Long projectId, Long statusId);

    void checkBeforeDeleteWorkLog(Long organizationId, Long projectId, Long statusId);

    /**
     * 根据工作项ID查询工时日志列表
     * @param projectId     项目ID
     * @param issueId       工作项ID
     * @param workLogVOList 基础返回值
     * @return              处理后的返回值
     */
    List<WorkLogVO> queryWorkLogListByIssueId(Long projectId, Long issueId, List<WorkLogVO> workLogVOList);

    /**
     * 根据工时日志ID查询工时日志
     * @param projectId     项目ID
     * @param logId         工时日志ID
     * @param workLog       基础返回值
     * @return              处理后的返回值
     */
    WorkLogVO queryWorkLogById(Long projectId, Long logId, WorkLogVO workLog);

    /**
     * 登记工时
     * @param projectId     项目ID
     * @param workLog       前端传参
     * @param workLogDTO    基础返回值
     * @return              处理后的返回值
     */
    WorkLogDTO createWorkLog(Long projectId, WorkLogVO workLog, WorkLogDTO workLogDTO);

    /**
     * 修改工时日志
     * @param projectId     项目ID
     * @param workLogId     工时日志ID
     * @param workLog       前端传参
     * @param workLogDTO    基础返回值
     * @return              处理后的返回值
     */
    WorkLogDTO updateWorkLog(Long projectId, Long workLogId, WorkLogVO workLog, WorkLogDTO workLogDTO);

    /**
     * 删除工时日志
     * @param projectId     项目ID
     * @param workLogId     工作日志ID
     */
    void deleteWorkLog(Long projectId, Long workLogId);
}
