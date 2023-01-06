package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.WorkLogDTO;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/9/5.
 * Email: fuqianghuang01@gmail.com
 */
public interface IWorkLogService {

    WorkLogDTO createBase(WorkLogDTO workLogDTO);

    /**
     * 根据项目ID和工作项ID删除工时日志, 触发webhook
     * @param projectId 项目ID
     * @param issueId   工作项ID
     */
    void deleteByProjectIdWithoutDataLog(Long projectId, Long issueId);

    /**
     * 根据项目ID和工时日志ID删除工时日志, 触发历史记录, 触发webhook
     * @param projectId 项目ID
     * @param logId     工时日志ID
     */
    void deleteBase(Long projectId,Long logId);

    /**
     * 根据项目ID和工时日志ID删除工时日志, 触发webhook
     * @param projectId 项目ID
     * @param logId     工时日志ID
     */
    void deleteBaseWithoutDataLog(Long projectId,Long logId);
}
