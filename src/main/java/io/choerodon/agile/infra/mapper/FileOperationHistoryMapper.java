package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.FileOperationHistoryDTO;
import io.choerodon.mybatis.common.BaseMapper;

import org.apache.ibatis.annotations.Param;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/2/25.
 * Email: fuqianghuang01@gmail.com
 */
public interface FileOperationHistoryMapper extends BaseMapper<FileOperationHistoryDTO> {

    FileOperationHistoryDTO queryLatestRecode(@Param("projectId") Long projectId, @Param("userId") Long userId, @Param("action") String action);

    /**
     * 查询组织层最后一次操作记录
     *
     * @param organizationId 组织id
     * @param action         操作名称
     * @param userId         用户id
     * @return 最后一次操作记录
     */
    FileOperationHistoryDTO queryOrgLatestRecode(@Param("organizationId") Long organizationId, @Param("userId") Long userId, @Param("action") String action);
}
