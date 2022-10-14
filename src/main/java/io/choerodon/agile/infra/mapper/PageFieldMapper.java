package io.choerodon.agile.infra.mapper;

import java.util.List;
import java.util.Set;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.infra.dto.PageFieldDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
public interface PageFieldMapper extends BaseMapper<PageFieldDTO> {
    /**
     * 组织层/项目层 根据页面编码和上下文获取页面字段列表
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param pageCode pageCode
     * @param context context
     * @return result
     */
    List<PageFieldDTO> listQuery(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("pageCode") String pageCode, @Param("context") String context);

    /**
     * 递减的情况下，查询较小的rank值
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param pageCode pageCode
     * @param rank rank
     * @return result
     */
    String queryRightRank(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("pageCode") String pageCode, @Param("rank") String rank);

    /**
     * 获取页面最小的rank
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param pageCode pageCode
     * @return result
     */
    String queryMinRank(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("pageCode") String pageCode);

    /**
     * 根据字段id查询页面字段
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param pageCode pageCode
     * @param fieldId fieldId
     * @return result
     */
    PageFieldDTO queryByFieldId(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("pageCode") String pageCode, @Param("fieldId") Long fieldId);

    /**
     * 初始化页面字段：批量创建
     *
     * @param organizationId organizationId
     * @param pageFields pageFields
     */
    void batchInsert(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("pageFields") List<PageFieldDTO> pageFields);

    /**
     * 复制组织层（页面字段）到项目层
     *
     * @param organizationId organizationId
     * @param projectId projectId
     */
    void copyOrgPageFieldToPro(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId);

    /**
     * 根据字段id删除所有页面字段
     *
     * @param fieldId fieldId
     */
    void deleteByFieldId(@Param("fieldId") Long fieldId);

    /**
     * 查所有的组织id
     * @return result
     */
    Set<Long> selectOrganizationIds();
}
