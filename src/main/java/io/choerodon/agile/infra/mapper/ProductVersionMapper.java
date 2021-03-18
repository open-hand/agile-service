package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.mybatis.common.BaseMapper;

import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * Created by jian_zhang02@163.com on 2018/5/14.
 */
public interface ProductVersionMapper extends BaseMapper<ProductVersionDTO> {

    List queryVersionIdsByProjectId(@Param("projectId") Long projectId,
                                    @Param("searchArgs") Map<String, Object> searchArgs,
                                    @Param("advancedSearchArgs") Map<String, Object> advancedSearchArgs, @Param("contents") List<String> contents);

    List<ProductVersionDTO> queryVersionByIds(@Param("projectId") Long projectId, @Param("versionIds") List<Long> versionIds);

    Boolean isRepeatName(@Param("projectId") Long projectId, @Param("name") String name);

    Boolean isNotReName(@Param("projectId") Long projectId, @Param("versionId") Long versionId, @Param("name") String name);

    List<ProductVersionDataDTO> queryVersionByProjectId(@Param("projectId") Long projectId);

    ProductVersionStatisticsDTO queryVersionStatisticsByVersionId(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    List<IssueDTO> queryIssueByVersionIdAndStatusCode(@Param("projectId") Long projectId, @Param("versionId") Long versionId, @Param("statusCode") String statusCode, @Param("filterStatusIds") List<Long> filterStatusIds, @Param("searchVO") SearchVO searchVO);

    List<IssueCountDTO> queryIssueCount(@Param("projectId") Long projectId, @Param("versionIds") List<Long> versionIds, @Param("statusIds") List<Long> statusIds);

    List<IssueCountDTO> queryNotEstimate(@Param("projectId") Long projectId, @Param("versionIds") List<Long> productVersionIds);

    List<IssueCountDTO> queryTotalEstimate(@Param("projectId") Long projectId, @Param("versionIds") List<Long> productVersionIds);

    int queryNotDoneIssueCount(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    List<ProductVersionNameDTO> queryPlanVersionNames(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    int issueToDestination(@Param("projectId") Long projectId, @Param("targetVersionId") Long targetVersionId, @Param("versionIssues") List<VersionIssueDTO> versionIssues, @Param("date") Date date, @Param("userId") Long userId);

    int releaseVersion(@Param("projectId") Long projectId, @Param("versionId") Long versionId, @Param("releaseDate") Date releaseDate);

    int queryIssueCountByApplyType(@Param("projectId") Long projectId, @Param("versionId") Long versionId, @Param("applyType") String applyType);

    List<ProductVersionNameDTO> queryVersionNames(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    List<VersionIssueDTO> queryIncompleteIssues(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    List<VersionIssueDTO> queryIssuesByRelationType(@Param("projectId") Long projectId, @Param("versionId") Long versionId, @Param("relationType") String relationType);

    List<ProductVersionNameDTO> queryNameByOptions(@Param("projectId") Long projectId, @Param("statusCodes") List<String> statusCodes);

    List<ProductVersionCommonDTO> listByProjectId(@Param("projectId") Long projectId);

    int deleteByVersionIds(@Param("projectId") Long projectId, @Param("versionIds") List<Long> versionIds);

    List<ProductVersionDTO> selectVersionRelsByIssueId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    List<Long> listIds();

    /**
     * 根据issueId和类型查询版本关联
     *
     * @param projectId    projectId
     * @param issueId      issueId
     * @param relationType relationType
     * @return ProductVersionDTO
     */
    List<ProductVersionDTO> queryVersionRelByIssueIdAndTypeArchivedExceptInfluence(@Param("projectId") Long projectId, @Param("issueId") Long issueId, @Param("relationType") String relationType);

    /**
     * 批量更新排序
     *
     * @param sequence  sequence
     * @param projectId projectId
     * @param add       add
     * @param versionId versionIdatu
     * @return int
     */
    int batchUpdateSequence(@Param("sequence") Integer sequence, @Param("projectId") Long projectId, @Param("add") Integer add, @Param("versionId") Long versionId);

    /**
     * 查询最大的序号
     *
     * @param projectId projectId
     * @return Integer
     */
    Integer queryMaxSequenceByProject(@Param("projectId") Long projectId);

    /**
     * 获取当前排序后的最大一个
     *
     * @param sequence  sequence
     * @param projectId projectId
     * @return Integer
     */
    Integer queryMaxAfterSequence(@Param("sequence") Integer sequence, @Param("projectId") Long projectId);

    /**
     * 获取当前排序前的最小一个
     *
     * @param sequence  sequence
     * @param projectId projectId
     * @return Integer
     */
    Integer queryMinBeforeSequence(@Param("sequence") Integer sequence, @Param("projectId") Long projectId);

    /**
     * 通过versionIds查询issue关系
     *
     * @param projectId  projectId
     * @param versionIds versionIds
     * @return VersionIssueDTO
     */
    List<VersionIssueDTO> queryIssueForLogByVersionIds(@Param("projectId") Long projectId, @Param("versionIds") List<Long> versionIds);

    /**
     * 查询版本关系中未完成的版本
     *
     * @param projectId projectId
     * @param versionId versionId
     * @return VersionIssueDTO
     */
    List<VersionIssueDTO> queryInCompleteIssueByVersionId(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    /**
     * 查询版本中issue关系
     *
     * @param projectId projectId
     * @param versionId versionId
     * @return VersionIssueDTO
     */
    List<VersionIssueDTO> queryVersionIssueByVersionId(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    /**
     * 查询版本中的issue中的状态统计
     *
     * @param statusIds statusIds
     * @param projectId projectId
     * @param versionId versionId
     * @return Integer
     */
    Integer queryStatusIssueCount(@Param("statusIds") List<Long> statusIds, @Param("projectId") Long projectId, @Param("versionId") Long versionId);

    List<TestVersionFixVO> queryByVersionId();

    /**
     * 查询产品版本关联的应用版本
     *
     * @param versionId          产品版本id
     * @param projectId          项目id
     * @param appVersionSearchVO 查询条件
     * @return 产品版本关联的应用版本
     */
    List<AppVersionVO> listAppVersionByOption(@Param("projectId") Long projectId, @Param("versionId") Long versionId, @Param("appVersionSearchVO") AppVersionSearchVO appVersionSearchVO);

    /**
     * 查询产品版本未关联的应用版本
     *
     * @param projectId          项目id
     * @param versionId          产品版本id
     * @param appVersionSearchVO 查询条件
     * @return 产品版本未关联的应用版本
     */
    List<AppVersionVO> listUnRelAppVersionByOption(@Param("projectId") Long projectId, @Param("versionId") Long versionId, @Param("appVersionSearchVO") AppVersionSearchVO appVersionSearchVO);

    /**
     * 查询产品版本关联的应用版本id
     *
     * @param versionId 产品版本id
     * @param projectId 项目id
     * @return 产品版本关联的应用版本
     */
    List<Long> listAppVersionIdByVersionId(@Param("projectId") Long projectId, @Param("versionId") Long versionId);

    /**
     * 查询应用版本关联的产品版本中的最小序列
     *
     * @param projectId     项目id
     * @param appVersionIds 应用版本id
     * @return 关联的产品版本中的最小序列
     */
    List<AppVersionRelProductSequenceVO> listMinSequenceByAppVersion(@Param("projectId") Long projectId, @Param("appVersionIds") List<Long> appVersionIds);
}
