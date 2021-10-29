package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.IssueProgressVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.dto.EpicWithInfoDTO;
import io.choerodon.agile.infra.dto.business.StoryMapStoryDTO;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface StoryMapMapper {

    List<Long> selectEpicIdsByProject(@Param("projectId") Long projectId, @Param("searchVO") SearchVO searchVO);

    List<EpicWithInfoDTO> selectEpicList(@Param("projectId") Long projectId, @Param("epicIds") List<Long> epicIds, @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs);

    List<StoryMapStoryDTO> selectStoryList(@Param("projectIds") Set<Long> projectIds, @Param("epicIds") List<Long> epicIds, @Param("searchVO") SearchVO searchVO, @Param("filterSql") String filterSql, @Param("assigneeFilterIds") List<Long> assigneeFilterIds);

    List<StoryMapStoryDTO> selectDemandStoryList(@Param("projectId") Long projectId, @Param("searchVO") SearchVO searchVO);

    List<IssueProgressVO> countEpicProgress(@Param("projectId") Long projectId, @Param("epicIds") List<Long> epicIds);

    List<IssueProgressVO> countStoryProgress(@Param("projectId") Long projectId, @Param("storyIds") List<Long> storyIds);
}
