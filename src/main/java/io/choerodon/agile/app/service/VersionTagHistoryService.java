package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.VersionTagHistoryVO;
import io.choerodon.agile.infra.dto.VersionTagHistoryDTO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/19 10:58
 */
public interface VersionTagHistoryService {

    /**
     * 创建tag操作记录
     * @param projectId 项目id
     * @param organizationId 组织id
     * @param versionId 发布版本id
     * @param action 动作
     * @return 创建的tag操作记录
     */
    VersionTagHistoryDTO createDefaultHistory(Long projectId,
                                              Long organizationId,
                                              Long versionId,
                                              String action,
                                              String versionType);

    /**
     * 更新tag操作记录状态
     * @param versionTagHistoryDTO 要更新的记录
     * @param status 要更新的状态
     */
    void updateStatus(VersionTagHistoryDTO versionTagHistoryDTO, String status);

    /**
     * 查询最近的tag历史
     * @param projectId 项目id
     * @param versionId 版本id
     * @return 最近的tag历史
     */
    VersionTagHistoryVO queryLatestHistory(Long projectId, Long versionId, String versionType);
}
