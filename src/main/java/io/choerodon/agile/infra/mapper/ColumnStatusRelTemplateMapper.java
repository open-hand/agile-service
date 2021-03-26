package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.ColumnStatusRelDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-03-26 11:49
 */
public interface ColumnStatusRelTemplateMapper  extends BaseMapper<ColumnStatusRelDTO> {
    List<Long> queryBoardTemplateStatusIds(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("boardTemplateId") Long boardTemplateId);
}
