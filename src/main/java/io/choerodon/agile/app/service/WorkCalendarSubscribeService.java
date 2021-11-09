package io.choerodon.agile.app.service;

import org.springframework.http.ResponseEntity;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * @author huaxin.deng@hand-china.com 2021-10-11 14:35:07
 */
public interface WorkCalendarSubscribeService {

    /**
     * 订阅工作日历
     * @param organizationId 组织id
     * @return uuid
     */
    String subscribe(Long organizationId);

    /**
     * 将订阅文件标记为已更新
     * @param projectId 项目id
     * @param issueId issueId
     * @param estimatedTimeChanged 是否更新预估时间
     * @param oldUserIds 更新前关联的人员
     */
    void handleWorkCalendarSubscribeChanged(Long projectId, Long issueId, boolean estimatedTimeChanged, List<Long> oldUserIds);

    /**
     * 获取订阅文件
     * @param organizationId 组织id
     * @param uuid uuid
     * @param httpResponse 请求响应
     * @return 文件内容
     */
    ResponseEntity<byte[]> downloadFile(Long organizationId, String uuid, HttpServletResponse httpResponse);

    /**
     * 订阅查询
     * @param organizationId 组织id
     * @return uuid
     */
    String query(Long organizationId);
}
