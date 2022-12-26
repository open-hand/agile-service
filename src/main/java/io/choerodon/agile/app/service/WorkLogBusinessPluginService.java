package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.WorkLogVO;

/**
 * 工时日志商业插件
 * @author gaokuo.dai@zknow.com 2022-12-26
 */
public interface WorkLogBusinessPluginService {


    void checkCreateWorkLog(Long organizationId, Long projectId, Long statusId);

    void checkDeleteWorkLog(Long organizationId, Long projectId, Long statusId);

    /**
     * 根据工作项ID查询工时日志列表
     * @param projectId     项目ID
     * @param issueId       工作项ID
     * @param workLogVOList 基础返回值
     * @return              处理后的返回值
     */
    List<WorkLogVO> queryWorkLogListByIssueId(Long projectId, Long issueId, List<WorkLogVO> workLogVOList);
}
