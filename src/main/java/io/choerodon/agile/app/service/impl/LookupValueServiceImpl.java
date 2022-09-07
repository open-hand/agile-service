package io.choerodon.agile.app.service.impl;


import io.choerodon.agile.api.vo.LookupTypeWithValuesVO;
import io.choerodon.agile.api.vo.LookupValueVO;
import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.app.service.BacklogExpandService;
import io.choerodon.agile.app.service.LookupValueService;
import io.choerodon.agile.infra.dto.LookupTypeWithValuesDTO;
import io.choerodon.agile.infra.dto.LookupValueDTO;
import io.choerodon.agile.infra.enums.LookupType;
import io.choerodon.agile.infra.enums.ObjectSchemeFieldContext;
import io.choerodon.agile.infra.mapper.LookupValueMapper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.*;
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
    protected LookupValueMapper lookupValueMapper;
    @Autowired
    protected ModelMapper modelMapper;

    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;

    @Autowired(required = false)
    private AgilePluginService agilePluginService;

    @Override
    public LookupTypeWithValuesVO queryLookupValueByCode(String typeCode, Long projectId) {
        LookupTypeWithValuesDTO typeWithValues = lookupValueMapper.queryLookupValueByCode(typeCode);
        if (LookupType.CONTEXT.equals(typeCode) && !ObjectUtils.isEmpty(projectId)) {
            List<LookupValueDTO> backlogs = filterBacklog(projectId, typeWithValues);
            List<LookupValueDTO> lookupValues = filterProjectType(projectId, typeWithValues);
            lookupValues.addAll(backlogs);
            typeWithValues.setLookupValues(lookupValues);
        }
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

    @Override
    public List<LookupValueDTO> queryByValueCodes(Set<String> valueCodes) {
        if (ObjectUtils.isEmpty(valueCodes)) {
            return Collections.emptyList();
        }
        return lookupValueMapper.selectByValueCodes(valueCodes);
    }

    protected List<LookupValueDTO> filterProjectType(Long projectId, LookupTypeWithValuesDTO typeWithValues) {
        if (agilePluginService != null) {
            return agilePluginService.filterProgramType(projectId, typeWithValues);
        } else {
            return typeWithValues
                    .getLookupValues()
                    .stream()
                    .filter(i -> ObjectSchemeFieldContext.NORMAL_PROJECT.contains(i.getValueCode()))
                    .collect(Collectors.toList());
        }
    }

    private List<LookupValueDTO> filterBacklog(Long projectId, LookupTypeWithValuesDTO typeWithValues) {
        if (backlogExpandService == null) {
            return new ArrayList<>();
        }
        Boolean backlogEnabled = backlogExpandService.enabled(projectId);
        List<LookupValueDTO> result = new ArrayList<>();
        if (Boolean.TRUE.equals(backlogEnabled)) {
            List<LookupValueDTO> lookupValues =
                    typeWithValues
                            .getLookupValues()
                            .stream()
                            .filter(i -> ObjectSchemeFieldContext.BACKLOG.equals(i.getValueCode()))
                            .collect(Collectors.toList());
            result.addAll(lookupValues);
        }
        return result;
    }
}