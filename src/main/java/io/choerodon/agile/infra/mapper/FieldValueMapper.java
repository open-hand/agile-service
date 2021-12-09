package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.FieldInstanceCountVO;
import io.choerodon.agile.infra.dto.FieldValueDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
public interface FieldValueMapper extends BaseMapper<FieldValueDTO> {

    List<FieldValueDTO> queryList(@Param("projectId") Long projectId, @Param("instanceId") Long instanceId, @Param("schemeCode") String schemeCode, @Param("fieldId") Long fieldId);

    void batchInsert(@Param("projectId") Long projectId, @Param("instanceId") Long instanceId, @Param("schemeCode") String schemeCode, @Param("fieldValues") List<FieldValueDTO> fieldValues);

    void deleteByOptionIds(@Param("fieldId") Long fieldId, @Param("optionIds") List<Long> optionIds);

    void deleteList(@Param("projectId") Long projectId, @Param("instanceId") Long instanceId, @Param("schemeCode") String schemeCode, @Param("fieldId") Long fieldId);

    List<Long> sortIssueIdsByFieldValue(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("fieldId") Long fieldId, @Param("sortSql") String sortSql, @Param("schemeCode") String schemeCode);

    List<FieldValueDTO> listByInstanceIdsAndFieldId(@Param("projectId")Long projectId,@Param("instanceIds") List<Long> instanceIds, @Param("schemeCode") String schemeCode,@Param("fieldId") Long fieldId);

    void deleteByInstanceIds(@Param("projectId") Long projectId, @Param("instanceIds") List<Long> instanceIds, @Param("schemeCode") String schemeCode, @Param("fieldId") Long fieldId);

    void batchInsertField(@Param("projectId") Long projectId,  @Param("schemeCode") String schemeCode, @Param("fieldValues") List<FieldValueDTO> fieldValues);

    List<Long> selectUserIdByField(@Param("projectId") Long projectId, @Param("userTypeList") List<String> userTypeList, @Param("issueId") Long issueId);

    List<FieldValueDTO> queryListByInstanceIds(@Param("projectIds") List<Long> projectIds, @Param("instanceIds") List<Long> instanceIds, @Param("schemeCode") String schemeCode, @Param("fieldId") Long fieldId);

    void updateProjectId(@Param("projectId") Long projectId, @Param("targetProjectId") Long targetProjectId, @Param("issueId") Long issueId, @Param("schemeCode") String schemeCode);

    List<FieldValueDTO> selectByFieldIds(@Param("projectId") Long projectId,
                                         @Param("instanceId") Long instanceId,
                                         @Param("schemeCode") String schemeCode,
                                         @Param("fieldIds") Set<Long> fieldIds);

    List<Long> queryInstanceByFieldIdAndIssueTypeId(@Param("projectIds") List<Long> projectIds,
                                                    @Param("schemeCode") String schemeCode,
                                                    @Param("fieldId") Long fieldId,
                                                    @Param("issueTypeId") Long issueTypeId);

    List<FieldInstanceCountVO> queryInstanceCountByFieldIds(@Param("projectIds") List<Long> projectIds,
                                                            @Param("schemeCode") String schemeCode,
                                                            @Param("fieldIds") List<Long> fieldIds,
                                                            @Param("issueTypeId") Long issueTypeId);
}
