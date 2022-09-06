package io.choerodon.agile.infra.mapper;

import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.agile.infra.dto.*;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;


/**
 * 敏捷开发code键值
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-15 09:40:27
 */
public interface LookupValueMapper extends BaseMapper<LookupValueDTO> {

    LookupTypeWithValuesDTO queryLookupValueByCode(String typeCode);

    String selectNameByValueCode(String valueCode);

    List<LookupValueDTO> selectByValueCodes(@Param("valueCodes") Set<String> valueCodes);
}