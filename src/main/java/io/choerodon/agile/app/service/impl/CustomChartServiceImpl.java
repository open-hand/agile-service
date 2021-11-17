package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.annotation.Resource;

import io.choerodon.agile.api.vo.CustomChartCreateVO;
import io.choerodon.agile.api.vo.CustomChartUpdateVO;
import io.choerodon.agile.api.vo.CustomChartVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.report.CustomChartDataVO;
import io.choerodon.agile.api.vo.report.CustomChartSearchVO;
import io.choerodon.agile.app.service.CustomChartService;
import io.choerodon.agile.app.service.ReportService;
import io.choerodon.agile.infra.dto.CustomChartDTO;
import io.choerodon.agile.infra.mapper.CustomChartMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.RedisUtil;
import io.choerodon.core.exception.CommonException;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/21 16:34
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class CustomChartServiceImpl implements CustomChartService {

    private static final String AGILE = "Agile::";
    private static final String CUSTOM_CHART = AGILE + "CustomChart";
    private static final String COLON = ":";

    @Resource
    private CustomChartMapper customChartMapper;
    @Autowired
    private ReportService reportService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private RedisUtil redisUtil;

    @Override
    public List<CustomChartVO> queryListByProject(Long projectId) {
        CustomChartDTO customChartRecord = new CustomChartDTO();
        customChartRecord.setProjectId(projectId);
        List<CustomChartDTO> customChartList = customChartMapper.select(customChartRecord);
        if (CollectionUtils.isEmpty(customChartList)) {
            return new ArrayList<>();
        }
        List<CustomChartVO> results = modelMapper.map(customChartList, new TypeToken<List<CustomChartVO>>() {
        }.getType());
        results.forEach(customChartVO -> {
            if (customChartVO.getSearchJson() != null) {
                customChartVO.setSearchJson(EncryptionUtils.handlerPersonFilterJson(customChartVO.getSearchJson(), true));
            }
        });
        return results;
    }

    @Override
    public CustomChartVO createCustomChart(Long projectId, CustomChartCreateVO customChartCreate) {
        CustomChartDTO customChartDTO = modelMapper.map(customChartCreate, CustomChartDTO.class);
        customChartDTO.setProjectId(projectId);
        customChartDTO.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        validAndSetJson(customChartDTO);
        if (customChartMapper.insertSelective(customChartDTO) != 1) {
            throw new CommonException("error.customChart.insert");
        }
        return queryById(customChartDTO.getId());
    }

    @Override
    public CustomChartVO updateCustomChart(Long projectId, Long customChartId, CustomChartUpdateVO customChartUpdate) {
        CustomChartDTO customChartDTO = modelMapper.map(customChartUpdate, CustomChartDTO.class);
        customChartDTO.setId(customChartId);
        deleteCustomChartRedisById(projectId, customChartId);
        if (customChartDTO.getAnalysisField() != null && customChartDTO.getAnalysisFieldPredefined() == null) {
            throw new CommonException("error.customChart.analysisFieldPredefinedNotNull");
        }
        customChartDTO.setProjectId(projectId);
        validAndSetJson(customChartDTO);
        if (customChartMapper.updateByPrimaryKeySelective(customChartDTO) != 1) {
            throw new CommonException("error.CustomChart.update");
        }
        return queryById(customChartId);
    }

    private void deleteCustomChartRedisById(Long projectId, Long customChartId) {
        CustomChartDTO customChart = queryCustomChartByProjectAndId(projectId, customChartId);
        CustomChartSearchVO customChartSearchVO = customChartToDataSearch(customChart);
        if (customChartSearchVO == null){
            return;
        }
        redisUtil.delete(CUSTOM_CHART + projectId + COLON + customChartSearchVO);
    }

    private CustomChartSearchVO customChartToDataSearch(CustomChartDTO customChart) {
        if (customChart == null) {
            return null;
        }
        CustomChartSearchVO customChartSearchVO = modelMapper.map(customChart, CustomChartSearchVO.class);
        if (!StringUtils.isBlank(customChart.getSearchJson())) {
            try {
                SearchVO searchVO = objectMapper.readValue(customChart.getSearchJson(), SearchVO.class);
                customChartSearchVO.setSearchVO(searchVO);
            } catch (IOException ioException) {
                throw new CommonException("error.customChart.searchJson.failed");
            }
        }
        return customChartSearchVO;
    }

    private void validAndSetJson(CustomChartDTO customChartDTO) {
        if (customChartDTO.getComparedField() != null && customChartDTO.getComparedFieldPredefined() == null) {
            throw new CommonException("error.customChart.comparedFieldPredefinedNotNull");
        }
        if (validNameRepeat(customChartDTO)) {
            throw new CommonException("error.customChart.name.repeat");
        }
        if (customChartDTO.getSearchJson() != null) {
            customChartDTO.setSearchJson(EncryptionUtils.handlerPersonFilterJson(customChartDTO.getSearchJson(), false));
        }
    }

    @Override
    public CustomChartVO queryCustomChartDetail(Long projectId, Long customChartId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        CustomChartDTO customChart = queryCustomChartByProjectAndId(projectId, customChartId);
        if (customChart == null) {
            return null;
        }
        CustomChartSearchVO customChartSearchVO = customChartToDataSearch(customChart);
        CustomChartDataVO customChartData = reportService.queryCustomChartData(customChartSearchVO, projectId, organizationId);
        CustomChartVO result = modelMapper.map(customChart, CustomChartVO.class);
        result.setCustomChartData(customChartData);
        if (!StringUtils.isBlank(result.getSearchJson())) {
            result.setSearchJson(EncryptionUtils.handlerPersonFilterJson(result.getSearchJson(), true));
        }
        return result;
    }

    @Override
    public Boolean checkName(Long projectId, String name) {
        CustomChartDTO customChart = new CustomChartDTO();
        customChart.setProjectId(projectId);
        customChart.setName(name);
        return validNameRepeat(customChart);
    }

    @Override
    public void deleteCustomChartById(Long customChartId, Long projectId) {
        deleteCustomChartRedisById(projectId, customChartId);
        CustomChartDTO customChartDTO = new CustomChartDTO();
        customChartDTO.setProjectId(projectId);
        customChartDTO.setId(customChartId);
        int isDelete = customChartMapper.delete(customChartDTO);
        if (isDelete != 1) {
            throw new CommonException("error.customChart.deleteById");
        }
    }

    private boolean validNameRepeat(CustomChartDTO customChartDTO) {
        CustomChartDTO customChartRecord = new CustomChartDTO();
        customChartRecord.setName(customChartDTO.getName());
        customChartRecord.setProjectId(customChartDTO.getProjectId());
        List<CustomChartDTO> results = customChartMapper.select(customChartRecord);
        if (CollectionUtils.isEmpty(results)) {
            return false;
        }
        if (results.size() > (customChartDTO.getId() == null ? 0 : 1)) {
            return true;
        }
        return !results.get(0).getId().equals(customChartDTO.getId());
    }

    private CustomChartDTO queryCustomChartByProjectAndId(Long projectId, Long id) {
        CustomChartDTO customChartRecord = new CustomChartDTO();
        customChartRecord.setId(id);
        customChartRecord.setProjectId(projectId);
        return customChartMapper.selectOne(customChartRecord);
    }

    private CustomChartVO queryById(Long id) {
        CustomChartVO result = modelMapper.map(customChartMapper.selectByPrimaryKey(id), CustomChartVO.class);
        if (result.getSearchJson() != null) {
            result.setSearchJson(EncryptionUtils.handlerPersonFilterJson(result.getSearchJson(), true));
        }
        return result;
    }
}
