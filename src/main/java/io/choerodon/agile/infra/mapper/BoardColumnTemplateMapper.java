package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.BoardColumnVO;
import io.choerodon.agile.api.vo.ColumnWithMaxMinNumVO;
import io.choerodon.agile.infra.dto.BoardColumnDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-03-26 11:45
 */
public interface BoardColumnTemplateMapper extends BaseMapper<BoardColumnDTO> {

    void updateMaxAndMinNumTemplate(@Param("organizationId") Long organizationId , @Param("columnInfo") ColumnWithMaxMinNumVO columnWithMaxMinNumVO);

    List<BoardColumnVO> listColumnAndStatusByBoardTemplateId(@Param("organizationId") Long organizationId, @Param("boardTemplateId") Long boardTemplateId);
}
