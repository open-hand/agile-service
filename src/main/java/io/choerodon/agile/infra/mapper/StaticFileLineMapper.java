package io.choerodon.agile.infra.mapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;

import io.choerodon.agile.infra.dto.StaticFileLineDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/13 16:07
 */
public interface StaticFileLineMapper extends BaseMapper<StaticFileLineDTO> {

    /**
     * 批量创建静态文件行记录
     *
     * @param lineList 要创建的静态文件行记录
     */
    void batchInsert(@Param("lineList") List<StaticFileLineDTO> lineList);
}
