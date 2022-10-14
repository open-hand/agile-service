package io.choerodon.agile.infra.mapper;

import java.util.List;
import java.util.Set;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.api.vo.PageConfigFieldVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldExtendDTO;
import io.choerodon.agile.infra.dto.PageFieldDTO;
import io.choerodon.mybatis.common.BaseMapper;


/**
 * @author superlee
 * @since 2020-08-10
 */
public interface ObjectSchemeFieldExtendMapper extends BaseMapper<ObjectSchemeFieldExtendDTO> {

    /**
     * 查询最小的排序值
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param issueType issueType
     * {@link ObjectSchemeFieldExtendMapper#selectMinRankByIssueTypeId(Long, Long, Long)}}
     * @return result
     */
    @Deprecated
    String selectMinRank(@Param("organizationId") Long organizationId,
                         @Param("projectId") Long projectId,
                         @Param("issueType") String issueType);

    /**
     * 查询最小的排序值
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param issueTypeId issueTypeId
     * @return result
     */
    String selectMinRankByIssueTypeId(@Param("organizationId") Long organizationId,
                                      @Param("projectId") Long projectId,
                                      @Param("issueTypeId") Long issueTypeId);



    /**
     * 查询组织层的扩展字段
     *
     * @param issueTypes issueTypes
     * @param organizationId organizationId
     * @param fieldId fieldId
     * {@link ObjectSchemeFieldExtendMapper#selectExtendFieldByOptions(List, Long, Long, Long)}}
     * @return result
     */
    @Deprecated
    List<ObjectSchemeFieldExtendDTO> selectExtendField(@Param("issueTypes") List<String> issueTypes,
                                                       @Param("organizationId") Long organizationId,
                                                       @Param("fieldId") Long fieldId,
                                                       @Param("projectId") Long projectId);


    /**
     * 查询组织层的扩展字段
     *
     * @param issueTypeIds issueTypeIds
     * @param organizationId organizationId
     * @param fieldId fieldId
     * @param projectId projectId
     * @return result
     */
    List<ObjectSchemeFieldExtendDTO> selectExtendFieldByOptions(@Param("issueTypeIds") List<Long> issueTypeIds,
                                                                @Param("organizationId") Long organizationId,
                                                                @Param("fieldId") Long fieldId,
                                                                @Param("projectId") Long projectId);


    List<ObjectSchemeFieldExtendDTO> selectExtendFieldsByOptions(@Param("issueTypeIds") List<Long> issueTypeIds,
                                                                 @Param("organizationId") Long organizationId,
                                                                 @Param("fieldIds") Set<Long> fieldIds,
                                                                 @Param("projectId") Long projectId);
    /**
     * 查询数量
     *
     * @param issueType issueType
     * @param organizationId organizationId
     * @param fieldId fieldId
     * @param projectId projectId
     * @return result
     */
    Integer selectExtendFieldCount(@Param("issueType") String issueType,
                                   @Param("organizationId") Long organizationId,
                                   @Param("fieldId") Long fieldId,
                                   @Param("projectId") Long projectId);

    /**
     * 查询页面配置数据
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param issueTypeId issueTypeId
     * @return result
     */
    List<PageConfigFieldVO> listConfigs(@Param("organizationId") Long organizationId,
                                        @Param("projectId") Long projectId,
                                        @Param("issueTypeId") Long issueTypeId);

    /**
     * 批量更新组织下的required字段
     *
     * @param issueType issueType
     * @param organizationId organizationId
     * @param fieldId fieldId
     * @param required required
     */
    void batchUpdateRequired(@Param("issueType") String issueType,
                             @Param("organizationId") Long organizationId,
                             @Param("fieldId") Long fieldId,
                             @Param("required") Boolean required);

    /**
     * 查询配置过的项目集合
     *
     * @param organizationId organizationId
     * @return result
     */
    Set<Long> selectProjectIdsByOrganizationId(@Param("organizationId") Long organizationId);

    /**
     * 查询targetRank之前最大的rank值
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param issueType issueType
     * @param targetRank targetRank
     * @return result
     */
    String selectPreviousRank(@Param("organizationId") Long organizationId,
                              @Param("projectId") Long projectId,
                              @Param("issueType") String issueType,
                              @Param("targetRank") String targetRank);

    /**
     * @param organizationId organizationId
     * @param projectId projectId
     * @param issueTypeId issueTypeId
     * @return result
     */
    List<ObjectSchemeFieldVO> unselected(@Param("organizationId") Long organizationId,
                                         @Param("projectId") Long projectId,
                                         @Param("issueTypeId") Long issueTypeId);

    /**
     * 批量插入接口
     *
     * @param objectSchemeFieldExtends objectSchemeFieldExtends
     */
    void batchInsert(@Param("insertList") List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtends);

    /**
     * 根据项目id和组织id以及界面类型查某个类型的字段
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param issueTypeId issueTypeId
     * @param created created
     * @param edited edited
     * @return result
     */
    List<PageFieldDTO> selectFields(@Param("organizationId") Long organizationId,
                                    @Param("projectId") Long projectId,
                                    @Param("issueTypeId") Long issueTypeId,
                                    @Param("created") Boolean created,
                                    @Param("edited") Boolean edited);

    List<ObjectSchemeFieldExtendDTO> selectExtendFields(@Param("organizationId") Long organizationId,
                                                        @Param("fieldId") Long fieldId,
                                                        @Param("projectId") Long projectId,
                                                        @Param("filterIssueTypeIds") List<Long> filterIssueTypeIds);

    /**
     * 查询根据code查询字段
     *
     * @param fieldCodes fieldCodes
     * @return result
     */
    List<PageConfigFieldVO> listConfigsByFieldCodes(@Param("fieldCodes") List<String> fieldCodes);

    /**
     * 查询各个问题类型的最小rank
     * @param organizationId 组织id
     * @param projectId 项目id
     * @return 各个问题类型的最小rank
     */
    List<ObjectSchemeFieldExtendDTO> selectIssueTypeMinRank(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId);

    void updateDefaultValueDeleteOption(@Param("fieldId") Long fieldId, @Param("optionId") Long optionId, @Param("organizationId") Long organizationId);
}
