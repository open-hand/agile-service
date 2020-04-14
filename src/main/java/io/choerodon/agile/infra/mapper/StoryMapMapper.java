package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.dto.EpicWithInfoDTO;
import io.choerodon.agile.infra.dto.StoryMapStoryDTO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface StoryMapMapper {

    List<Long> selectEpicIdsByProject(@Param("projectId") Long projectId);

    List<EpicWithInfoDTO> selectEpicList(@Param("projectId") Long projectId, @Param("epicIds") List<Long> epicIds);

    List<StoryMapStoryDTO> selectStoryList(@Param("projectId") Long projectId, @Param("epicIds") List<Long> epicIds, @Param("searchVO") SearchVO searchVO);

    List<StoryMapStoryDTO> selectDemandStoryList(@Param("projectId") Long projectId, @Param("searchVO") SearchVO searchVO);
}
