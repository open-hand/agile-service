package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.PublishVersionTagHistoryVO;
import io.choerodon.agile.infra.dto.PublishVersionTagHistoryDTO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/19 10:58
 */
public interface PublishVersionTagHistoryService {

    /**
     * 创建tag操作记录
     * @param projectId 项目id
     * @param organizationId 组织id
     * @param publishVersionId 发布版本id
     * @param action 动作
     * @return 创建的tag操作记录
     */
    PublishVersionTagHistoryDTO createDefaultHistory(Long projectId, Long organizationId, Long publishVersionId, String action);

    /**
     * 更新tag操作记录状态
     * @param publishVersionTagHistoryDTO 要更新的记录
     * @param status 要更新的状态
     */
    void updateStatus(PublishVersionTagHistoryDTO publishVersionTagHistoryDTO, String status);

    /**
     * 查询最近的tag历史
     * @param projectId 项目id
     * @param publishVersionId 发布版本id
     * @return 最近的tag历史
     */
    PublishVersionTagHistoryVO queryLatestHistory(Long projectId, Long publishVersionId);
}
