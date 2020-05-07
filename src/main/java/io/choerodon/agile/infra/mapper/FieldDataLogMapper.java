package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.FieldDataLogCreateVO;
import io.choerodon.agile.infra.dto.FieldDataLogDTO;
import io.choerodon.mybatis.common.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/6/19
 */
public interface FieldDataLogMapper extends Mapper<FieldDataLogDTO> {
    List<FieldDataLogDTO> queryByInstanceId(@Param("projectId") Long projectId, @Param("schemeCode") String schemeCode, @Param("instanceId") Long instanceId);

    void batchInsert(@Param("projectId") Long projectId, @Param("schemeCode") String schemeCode, @Param("list") List<FieldDataLogCreateVO> list, @Param("userId") Long userId);
}
