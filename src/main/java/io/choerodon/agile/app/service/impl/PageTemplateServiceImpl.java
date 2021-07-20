package io.choerodon.agile.app.service.impl;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.FieldOptionService;
import io.choerodon.agile.app.service.PageTemplateService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.infra.dto.IssueTypeFieldDTO;
import io.choerodon.agile.infra.mapper.FieldCascadeRuleMapper;
import io.choerodon.agile.infra.mapper.IssueTypeFieldMapper;
import io.choerodon.agile.infra.utils.FieldValueUtil;
import io.choerodon.core.exception.CommonException;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/20 16:26
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PageTemplateServiceImpl implements PageTemplateService {

    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private FieldOptionService optionService;
    @Resource
    private FieldCascadeRuleMapper fieldCascadeRuleMapper;
    @Resource
    private IssueTypeFieldMapper issueTypeFieldMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public PageTemplateVO queryPageTemplate(Long organizationId, Long projectId, Long issueTypeId) {
        if (ObjectUtils.isEmpty(issueTypeId)) {
            throw new CommonException("error.issue.type.not.existed");
        }
        PageTemplateVO result = new PageTemplateVO();
        List<PageConfigFieldVO> pageConfigFields = objectSchemeFieldService.queryPageConfigFields(organizationId, projectId, issueTypeId);
        //处理默认值
        processDefaultValue(pageConfigFields, organizationId, projectId);
        List<PageTemplateFieldVO> pageTemplateFieldList = modelMapper.map(pageConfigFields, new TypeToken<List<PageTemplateFieldVO>>(){}.getType());
        result.setFields(pageTemplateFieldList);

        //设置级联说明
        setPageTemplateFieldCascadeRuleDes(issueTypeId, projectId, pageTemplateFieldList);
        //设置IssueTypeField
        setIssueTypeField(projectId, issueTypeId, result);
        return result;
    }

    private void setPageTemplateFieldCascadeRuleDes(Long issueTypeId, Long projectId, List<PageTemplateFieldVO> pageTemplateFieldList) {
        List<FieldCascadeRuleDesVO> fieldCascadeRuleDesList = fieldCascadeRuleMapper.selectFieldCascadeRuleDesByIssueTypeId(issueTypeId, projectId);
        Map<Long, List<FieldCascadeRuleDesVO>> cascadeRuleMap = fieldCascadeRuleDesList.stream().collect(Collectors.groupingBy(FieldCascadeRuleDesVO::getFieldId));
        pageTemplateFieldList.forEach(pageTemplateFieldVO -> pageTemplateFieldVO.setFieldCascadeRuleDesList(cascadeRuleMap.get(pageTemplateFieldVO.getFieldId())));
    }

    private void setIssueTypeField(Long projectId, Long issueTypeId, PageTemplateVO result) {
        IssueTypeFieldDTO dto = new IssueTypeFieldDTO();
        dto.setIssueTypeId(issueTypeId);
        dto.setProjectId(projectId);
        List<IssueTypeFieldDTO> list = issueTypeFieldMapper.select(dto);
        if (!list.isEmpty()) {
            result.setIssueTypeFieldVO(modelMapper.map(list.get(0), IssueTypeFieldVO.class));
        }
    }

    private void processDefaultValue(List<PageConfigFieldVO> pageConfigFields,
                                     Long organizationId,
                                     Long projectId) {
        List<PageFieldViewVO> pageFieldViews = new ArrayList<>();
        pageConfigFields.forEach(p -> {
            PageFieldViewVO vo = new PageFieldViewVO();
            vo.setFieldId(p.getFieldId());
            vo.setDefaultValue(p.getDefaultValue());
            vo.setFieldType(p.getFieldType());
            vo.setExtraConfig(p.getExtraConfig());
            vo.setFieldCode(p.getFieldCode());
            pageFieldViews.add(vo);
        });
        optionService.fillOptions(organizationId, null, pageFieldViews);
        objectSchemeFieldService.setDefaultValueObjs(pageFieldViews, projectId, organizationId);
        FieldValueUtil.handleDefaultValue(pageFieldViews);
        Map<Long, PageFieldViewVO> pageFieldViewMap =
                pageFieldViews.stream().collect(Collectors.toMap(PageFieldViewVO::getFieldId, x -> x));
        pageConfigFields.forEach(p -> {
            Long fieldId = p.getFieldId();
            PageFieldViewVO vo = pageFieldViewMap.get(fieldId);
            if (!ObjectUtils.isEmpty(vo)) {
                p.setDefaultValue(vo.getDefaultValue());
                p.setDefaultValueObj(vo.getDefaultValueObj());
                p.setFieldOptions(vo.getFieldOptions());
                p.setDefaultValueObjs(vo.getDefaultValueObjs());
            }
        });
    }
}
