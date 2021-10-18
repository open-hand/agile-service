package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.IssueComponentBriefVO;
import io.choerodon.agile.infra.dto.ComponentForListDTO;
import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.agile.infra.dto.IssueComponentDTO;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */
public interface IssueComponentMapper extends BaseMapper<IssueComponentDTO> {

    /**
     * 重名校验
     *
     * @param name      name
     * @param projectId projectId
     * @return Boolean
     */
    Boolean checkNameExist(@Param("name") String name, @Param("projectId") Long projectId);

    /**
     * 根据名称和项目id查询模块id
     *
     * @param name      name
     * @param projectId projectId
     * @return Long
     */
    Long queryComponentIdByNameAndProjectId(@Param("name") String name, @Param("projectId") Long projectId);


    List queryIssuesByComponentId(@Param("projectId") Long projectId, @Param("componentId") Long componentId);

    /**
     * 根据参数查询模块id列表
     *
     * @param projectIds          projectIds
     * @param noIssueTest        noIssueTest
     * @param componentId        componentId
     * @param searchArgs         searchArgs
     * @param advancedSearchArgs advancedSearchArgs
     * @return Long
     */
    List<ComponentForListDTO> queryComponentByOption(@Param("projectIds") List<Long> projectIds, @Param("noIssueTest") Boolean noIssueTest,
                                                     @Param("componentId") Long componentId, @Param("searchArgs") Map<String, Object> searchArgs,
                                                     @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs, @Param("contents") List<String> contents);

    List<ComponentForListDTO> queryComponentWithIssueNum(@Param("projectId") Long projectId, @Param("componentId") Long componentId,
                                                         @Param("noIssueTest") Boolean noIssueTest);

    List<IssueComponentDTO> selectByProjectId(@Param("projectId") Long projectId);

    String queryMaxRank(@Param("projectId") Long projectId);

    List<IssueComponentDTO> querNoRankList(@Param("projectId") Long projectId);

    String queryRank(@Param("projectId") Long projectId, @Param("outsetId") Long outsetId);

    String queryLeftRank(@Param("projectId") Long projectId, @Param("rightRank") String rightRank);

    String queryRightRank(@Param("projectId") Long projectId, @Param("leftRank") String leftRank);

    void updateRank(@Param("projectId") Long projectId, @Param("issueComponentDTO") IssueComponentDTO issueComponentDTO);

    List<IssueComponentBriefVO> listByIssueIds(@Param("projectIds") Set<Long> projectIds, @Param("issueIds") List<Long> issueIds);
}
