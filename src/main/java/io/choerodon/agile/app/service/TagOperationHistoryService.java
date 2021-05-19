package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.TagOperationHistoryVO;
import io.choerodon.agile.infra.dto.TagOperationHistoryDTO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/19 10:58
 */
public interface TagOperationHistoryService {

    /**
     * 创建tag操作记录
     * @param projectId 项目id
     * @param organizationId 组织id
     * @param publishVersionId 发布版本id
     * @param action 动作
     * @return 创建的tag操作记录
     */
    TagOperationHistoryDTO createDefaultHistory(Long projectId, Long organizationId, Long publishVersionId, String action);

    /**
     * 更新tag操作记录状态
     * @param tagOperationHistoryDTO 要更新的记录
     * @param status 要更新的状态
     * @param message 更新的处理信息
     */
    void updateStatus(TagOperationHistoryDTO tagOperationHistoryDTO, String status, String message);

    /**
     * 查询最近的tag历史
     * @param projectId 项目id
     * @param publishVersionId 发布版本id
     * @return 最近的tag历史
     */
    TagOperationHistoryVO queryLatestHistory(Long projectId, Long publishVersionId);
}
