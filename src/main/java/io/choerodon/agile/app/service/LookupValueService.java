package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.LookupTypeWithValuesVO;
import io.choerodon.agile.infra.dto.LookupValueDTO;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 敏捷开发code键值
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-15 09:40:27
 */
public interface LookupValueService {

    LookupTypeWithValuesVO queryLookupValueByCode(String typeCode, Long projectId);

    LookupTypeWithValuesVO queryConstraintLookupValue();

    Map<String, String> queryMapByTypeCode(String typeCode);

    List<LookupValueDTO> queryByValueCodes(Set<String> valueCodes);

}