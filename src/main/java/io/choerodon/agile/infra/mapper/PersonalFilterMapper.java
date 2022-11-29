package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.PersonalFilterDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/2/25
 */
public interface PersonalFilterMapper extends BaseMapper<PersonalFilterDTO> {
    List<PersonalFilterDTO> queryByProjectIdAndUserId(@Param("organizationId") Long organizationId,
                                                      @Param("projectId") Long projectId,
                                                      @Param("userId") Long userId,
                                                      @Param("searchStr") String searchStr,
                                                      @Param("filterTypeCode") String filterTypeCode,
                                                      @Param("version") String version);

    int updateDefault(@Param("organizationId") Long organizationId,
                      @Param("projectId") Long projectId,
                      @Param("userId") Long userId,
                      @Param("isDefault") Boolean isDefault,
                      @Param("filterId") Long filterId,
                      @Param("filterTypeCode") String filterTypeCode);
}
