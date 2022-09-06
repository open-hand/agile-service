package io.choerodon.agile.infra.mapper;

import java.util.List;
import java.util.Set;

import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import io.choerodon.agile.api.vo.IssueTypeSearchVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.infra.dto.IssueTypeDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author shinan.chen
 * @date 2018/8/8
 */
@Component
public interface IssueTypeMapper extends BaseMapper<IssueTypeDTO> {

    List<IssueTypeDTO> queryBySchemeId(@Param("organizationId") Long organizationId,
                                       @Param("projectId") Long projectId,
                                       @Param("schemeId") Long schemeId,
                                       @Param("onlyEnabled") boolean onlyEnabled);

    List<IssueTypeDTO> queryByApplyTypes(@Param("organizationId") Long organizationId,
                                         @Param("projectId") Long projectId,
                                         @Param("applyTypes") List<String> applyTypes,
                                         @Param("onlyEnabled") boolean onlyEnabled,
                                         @Param("schemeType") String schemeType);


    /**
     * 根据id查询issue type
     *
     * @param organizationIds organizationIds
     * @return result
     */
    List<IssueTypeDTO> selectSystemIssueTypeByOrganizationIds(@Param("organizationIds") Set<Long> organizationIds);

    /**
     * 根据条件查询issueType
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param issueTypeSearchVO issueTypeSearchVO
     * @return result
     */
    List<IssueTypeVO> selectByOptions(@Param("organizationId") Long organizationId,
                                      @Param("projectId") Long projectId,
                                      @Param("issueTypeSearchVO") IssueTypeSearchVO issueTypeSearchVO);

    /**
     * 根据reference_id查询
     *
     * @param ids ids
     * @param organizationId organizationId
     * @return result
     */
    List<IssueTypeDTO> selectByReferenceId(@Param("ids") Set<Long> ids,
                                           @Param("organizationId") Long organizationId);

    /**
     * 查该项目可以引用的问题类型
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @return result
     */
    List<IssueTypeVO> selectEnableReference(@Param("organizationId") Long organizationId,
                                            @Param("projectId") Long projectId);

    /**
     * 查询问题类型，如果有别名使用别名
     *
     * @param issueTypeId issueTypeId
     * @param projectId projectId
     * @return result
     */
    IssueTypeDTO selectWithAlias(@Param("issueTypeId") Long issueTypeId,
                                 @Param("projectId") Long projectId);

    /**
     * 根据问题类型id和项目id查询问题类型
     *
     * @param issueTypeIds issueTypeIds
     * @param projectIds projectIds
     * @return result
     */
    List<IssueTypeDTO> selectWithAliasByIds(@Param("issueTypeIds") Set<Long> issueTypeIds,
                                            @Param("projectIds") Set<Long> projectIds,
                                            @Param("organizationId") Long organizationId);

    /**
     * 根据项目ids查询问题类型
     * @param organizationId organizationId
     * @param projectIds projectIds
     * @return result
     */
    List<IssueTypeVO> selectByProjectIds(@Param("organizationId") Long organizationId,
                                         @Param("projectIds") List<Long> projectIds);

    /**
     * 根据项目ids查询问题类型
     * @param organizationId organizationId
     * @param projectIds projectIds
     * @return result
     */
    List<IssueTypeVO> selectProjectIssueTypeByOptions(@Param("organizationId") Long organizationId,
                                                      @Param("projectIds") List<Long> projectIds,
                                                      @Param("issueTypeSearchVO") IssueTypeSearchVO issueTypeSearchVO);

}
