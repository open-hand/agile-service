package io.choerodon.agile.infra.mapper;

import java.util.List;
import java.util.Set;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.api.vo.DataLogQueryVO;
import io.choerodon.agile.api.vo.FieldDataLogCreateVO;
import io.choerodon.agile.api.vo.business.AllDataLogVO;
import io.choerodon.agile.infra.dto.FieldDataLogDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author shinan.chen
 * @since 2019/6/19
 */
public interface FieldDataLogMapper extends BaseMapper<FieldDataLogDTO> {
    List<FieldDataLogDTO> queryByInstanceId(@Param("projectId") Long projectId, @Param("schemeCode") String schemeCode, @Param("instanceId") Long instanceId);

    void batchInsert(@Param("projectId") Long projectId, @Param("schemeCode") String schemeCode, @Param("list") List<FieldDataLogCreateVO> list, @Param("userId") Long userId);

    void deleteByInstanceIdAndFieldIds(@Param("projectId") Long projectId, @Param("instanceId") Long instanceId, @Param("schemeCode") String schemeCode, @Param("fieldIds") Set<Long> fieldIds);

    void updateProjectId(@Param("projectId") Long projectId, @Param("targetProjectId") Long targetProjectId, @Param("instanceId") Long instanceId, @Param("schemeCode") String schemeCode);

    void deleteByInstanceIdsAndFieldIds(@Param("projectId") Long projectId,
                                        @Param("instanceIds") List<Long> instanceIds,
                                        @Param("schemeCode") String schemeCode,
                                        @Param("fieldIds") List<Long> fieldIds);

    /**
     * 查询自定义字段操作历史
     * @param projectId 项目id
     * @param dataLogQueryVO 查询参数
     * @param containBacklog 是否包含需求
     * @param containIssue containIssue
     * @return 自定义字段操作历史
     */
    List<AllDataLogVO> listFdDataLogByProjectId(
            @Param("projectId") Long projectId,
            @Param("dataLogQueryVO") DataLogQueryVO dataLogQueryVO,
            @Param("containBacklog") boolean containBacklog,
            @Param("containIssue") boolean containIssue);
}
