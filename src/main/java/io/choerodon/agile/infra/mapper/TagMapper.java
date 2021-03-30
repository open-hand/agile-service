package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.TagVO;
import io.choerodon.agile.infra.dto.TagDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-25
 */
public interface TagMapper extends BaseMapper<TagDTO> {

    List<TagVO> selectByProjectIds(@Param("projectIds") Set<Long> projectIds);
}
