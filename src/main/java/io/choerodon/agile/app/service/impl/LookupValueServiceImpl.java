package io.choerodon.agile.app.service.impl;


import io.choerodon.agile.api.vo.LookupTypeWithValuesVO;
import io.choerodon.agile.api.vo.LookupValueVO;
import io.choerodon.agile.app.service.LookupValueService;
import io.choerodon.agile.infra.dto.LookupTypeWithValuesDTO;
import io.choerodon.agile.infra.dto.LookupValueDTO;
import io.choerodon.agile.infra.mapper.LookupValueMapper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 敏捷开发code键值
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-15 09:40:27
 */
@Service
public class LookupValueServiceImpl implements LookupValueService {

    @Autowired
    private LookupValueMapper lookupValueMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public LookupTypeWithValuesVO queryLookupValueByCode(String typeCode) {
        LookupTypeWithValuesDTO typeWithValues = lookupValueMapper.queryLookupValueByCode(typeCode);
        LookupTypeWithValuesVO result = modelMapper.map(typeWithValues, new TypeToken<LookupTypeWithValuesVO>() {
        }.getType());
        result.setLookupValues(modelMapper.map(typeWithValues.getLookupValues(), new TypeToken<List<LookupValueVO>>() {
        }.getType()));
        return result;
    }

    @Override
    public LookupTypeWithValuesVO queryConstraintLookupValue() {
        LookupTypeWithValuesDTO typeWithValues = lookupValueMapper.queryLookupValueByCode("constraint");
        LookupTypeWithValuesVO result = modelMapper.map(typeWithValues, LookupTypeWithValuesVO.class);
        result.setLookupValues(modelMapper.map(typeWithValues.getLookupValues(), new TypeToken<List<LookupValueVO>>() {
        }.getType()));
        return result;
    }

    @Override
    public Map<String, String> queryMapByTypeCode(String typeCode) {
        LookupValueDTO lookupValueDTO = new LookupValueDTO();
        lookupValueDTO.setTypeCode(typeCode);
        List<LookupValueDTO> lookupValueDTOList = lookupValueMapper.select(lookupValueDTO);
        return lookupValueDTOList.stream().collect(Collectors.toMap(LookupValueDTO::getValueCode, LookupValueDTO::getName));
    }
}