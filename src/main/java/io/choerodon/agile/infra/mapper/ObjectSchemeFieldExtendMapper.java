package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.PageConfigVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldExtendDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;


/**
 * @author superlee
 * @since 2020-08-10
 */
public interface ObjectSchemeFieldExtendMapper extends BaseMapper<ObjectSchemeFieldExtendDTO> {

    /**
     * 查询最小的排序值
     *
     * @param organizationId
     * @param projectId
     * @param issueType
     * @return
     */
    String selectMinRank(@Param("organizationId") Long organizationId,
                         @Param("projectId") Long projectId,
                         @Param("issueType") String issueType);

    /**
     * 查询组织层的扩展字段
     *
     * @param issueType
     * @param organizationId
     * @param fieldId
     * @return
     */

    List<ObjectSchemeFieldExtendDTO> selectOrganizationExtendField(@Param("issueType") String issueType,
                                                                   @Param("organizationId") Long organizationId,
                                                                   @Param("fieldId") Long fieldId);

    /**
     * 查询页面配置数据
     * @param organizationId
     * @param projectId
     * @param issueType
     * @return
     */
    List<PageConfigVO> listConfigs(@Param("organizationId") Long organizationId,
                                   @Param("projectId") Long projectId,
                                   @Param("issueType") String issueType);

}
