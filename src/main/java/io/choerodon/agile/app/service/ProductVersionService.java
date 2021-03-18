package io.choerodon.agile.app.service;

import java.util.Date;
import java.util.List;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.infra.dto.ProductVersionDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * Created by jian_zhang02@163.com on 2018/5/14.
 */
public interface ProductVersionService {

    ProductVersionDetailVO createVersion(Long projectId, ProductVersionCreateVO versionCreateDTO);

    Boolean deleteVersion(Long projectId, Long versionId, Long targetVersionId);

    ProductVersionDetailVO updateVersion(Long projectId, Long versionId, ProductVersionUpdateVO versionUpdateDTO, List<String> fieldList);

    Page<ProductVersionPageVO> queryByProjectId(Long projectId, PageRequest pageRequest, SearchVO searchVO);

    Boolean repeatName(Long projectId, String name);

    List<ProductVersionDataVO> queryVersionByProjectId(Long projectId);

    ProductVersionStatisticsVO queryVersionStatisticsByVersionId(Long projectId, Long versionId);

    List<IssueListVO> queryIssueByVersionIdAndStatusCode(Long projectId, Long versionId, String statusCode, Long organizationId, SearchVO searchVO);

    VersionMessageVO queryReleaseMessageByVersionId(Long projectId, Long versionId);

    ProductVersionDetailVO releaseVersion(Long projectId, ProductVersionReleaseVO productVersionRelease);

    ProductVersionDetailVO revokeReleaseVersion(Long projectId, Long versionId);

    VersionMessageVO queryDeleteMessageByVersionId(Long projectId, Long versionId);

    List<ProductVersionNameVO> queryNameByOptions(Long projectId, List<String> statusCodes);

    List<ProductVersionVO> listByProjectId(Long projectId);

    ProductVersionDetailVO archivedVersion(Long projectId, Long versionId);

    ProductVersionDetailVO revokeArchivedVersion(Long projectId, Long versionId);

    ProductVersionDetailVO queryVersionByVersionId(Long projectId, Long versionId);

    List<Long> listIds(Long projectId);

    ProductVersionPageVO dragVersion(Long projectId, VersionSequenceVO versionSequenceVO);

    VersionIssueCountVO queryByCategoryCode(Long projectId, Long versionId);

    ProductVersionDTO createBase(ProductVersionDTO versionDTO);

    ProductVersionDTO updateByFieldList(ProductVersionDTO versionDTO, List<String> fieldList);

    Boolean release(Long projectId, Long versionId, Date releaseDate);

    ProductVersionDTO updateBase(ProductVersionDTO versionDTO);

    int deleteByVersionIds(Long projectId, List<Long> versionIds);

    int batchUpdateSequence(Integer sequence, Long projectId, Integer add, Long versionId);

    List<TestVersionFixVO> queryByVersionId();

    /**
     * 查询产品版本关联的应用版本
     *
     * @param versionId 产品版本id
     * @param projectId 项目id
     * @param appVersionSearchVO 查询条件
     * @return 产品版本关联的应用版本
     */
    List<AppVersionVO> listAppVersionByOption(Long projectId, Long versionId, AppVersionSearchVO appVersionSearchVO);

    /**
     * 查询产品版本未关联的应用版本
     *
     * @param projectId 项目id
     * @param versionId 产品版本id
     * @param appVersionSearchVO 查询条件
     * @param pageRequest 分页参数
     * @return 产品版本未关联的应用版本
     */
    Page<AppVersionVO> listUnRelAppVersionByOption(Long projectId, Long versionId, AppVersionSearchVO appVersionSearchVO, PageRequest pageRequest);

    /**
     * 创建产品版本与应用版本的关联关系
     * @param projectId 项目id
     * @param versionId 产品版本id
     * @param productRelAppVersion 应用版本id
     * @return 产品版本下关联的应用版本
     */
    List<AppVersionVO> createRelAppVersion(Long projectId, Long versionId, ProductVersionRelAppVersionVO productRelAppVersion);

    /**
     * 查询产品版本项目下关联的应用版本下已完成故事
     * @param projectId 项目id
     * @param versionId 产品版本id
     * @param searchVO 查询参数
     * @return 产品版本项目下关联的应用版本下已完成故事
     */
    List<IssueListFieldKVVO> listRelStoryByOption(Long projectId, Long versionId, SearchVO searchVO);

    /**
     * 查询产品版本项目下关联的应用版本下已完成缺陷
     * @param projectId 项目id
     * @param versionId 产品版本id
     * @param searchVO 查询参数
     * @return 产品版本项目下关联的应用版本下已完成缺陷
     */
    List<IssueListFieldKVVO> listRelBugByOption(Long projectId, Long versionId, SearchVO searchVO);

    /**
     * 创建应用版本并关联产品版本
     * @param projectId 项目id
     * @param versionId 版本id
     * @param appVersionCreateList 创建应用版本并关联产品版本
     * @return 产品版本关联的应用版本
     */
    List<AppVersionVO> createAndRelAppVersion(Long projectId, Long versionId, List<AppVersionCreateVO> appVersionCreateList);


    /**
     * 删除产品版本下应用版本和问题的关系
     * @param projectId 项目id
     * @param versionId 应用版本
     * @param issueId 问题id
     */
    void deleteIssueRel(Long projectId, Long versionId, Long issueId);
}
