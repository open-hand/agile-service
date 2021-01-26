package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.IssueTypeSearchVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.infra.dto.IssueTypeDTO;
import io.choerodon.agile.infra.dto.IssueTypeWithInfoDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;

/**
 * @author shinan.chen
 * @date 2018/8/8
 */
@Component
public interface IssueTypeMapper extends BaseMapper<IssueTypeDTO> {

    List<IssueTypeDTO> queryBySchemeId(@Param("organizationId") Long organizationId, @Param("schemeId") Long schemeId);

    List<IssueTypeDTO> queryByOrgId(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId);


    List<Long> selectIssueTypeIds(@Param("organizationId") Long organizationId, @Param("issueTypeSearchVO") IssueTypeSearchVO issueTypeSearchVO);

    List<IssueTypeWithInfoDTO> queryIssueTypeList(@Param("organizationId") Long organizationId, @Param("issueTypeIds") List<Long> issueTypeIds);

    /**
     * 根据id查询issue type
     *
     * @param organizationIds
     * @return
     */
    List<IssueTypeDTO> selectByOrganizationIds(@Param("organizationIds") Set<Long> organizationIds);

    /**
     * 根据typeCode查询issue type
     *
     * @param typeCode
     * @return
     */
    List<IssueTypeDTO> selectByTypeCode(@Param("typeCode") String typeCode);

    /**
     * 根据条件查询issueType
     *
     * @param organizationId
     * @param projectId
     * @param issueTypeSearchVO
     * @return
     */
    List<IssueTypeVO> selectByOptions(@Param("organizationId") Long organizationId,
                                      @Param("projectId") Long projectId,
                                      @Param("issueTypeSearchVO") IssueTypeSearchVO issueTypeSearchVO);

    /**
     * 根据reference_id查询
     *
     * @param ids
     * @param organizationId
     * @return
     */
    List<IssueTypeDTO> selectByReferenceId(@Param("ids") Set<Long> ids,
                                           @Param("organizationId") Long organizationId);

    /**
     * 查该项目可以引用的问题类型
     *
     * @param organizationId
     * @param projectId
     * @return
     */
    List<IssueTypeVO> selectEnableReference(@Param("organizationId") Long organizationId,
                                             @Param("projectId") Long projectId);

    /**
     * 根据条件查询问题类型id
     * @param issueTypeCodes
     * @param organizationId
     * @param projectId
     * @param source
     * @return
     */
    List<Long> selectIssueTypeIdsByOptions(@Param("issueTypeCodes") List<String> issueTypeCodes,
                                           @Param("organizationId") Long organizationId,
                                           @Param("projectId") Long projectId,
                                           @Param("source") String source);
}
