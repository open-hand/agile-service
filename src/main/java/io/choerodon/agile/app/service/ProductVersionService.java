package io.choerodon.agile.app.service;

import java.util.Date;
import java.util.List;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.infra.dto.ProductVersionDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * Created by jian_zhang02@163.com on 2018/5/14.
 */
public interface ProductVersionService {

    String VERSION_STATUS_CODE_PLANNING = "version_planning";
    String VERSION_STATUS_CODE_RELEASED = "released";
    String VERSION_STATUS_CODE_ARCHIVED = "archived";
    String VERSION_RELATION_TYPE_FIX = "fix";
    String VERSION_RELATION_TYPE_INFLUENCE = "influence";

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

}
