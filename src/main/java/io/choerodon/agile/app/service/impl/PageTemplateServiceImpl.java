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
    @Resource
    private FieldCascadeRuleMapper fieldCascadeRuleMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public PageTemplateVO queryPageTemplate(Long organizationId, Long projectId, Long issueTypeId) {
        if (ObjectUtils.isEmpty(issueTypeId)) {
            throw new CommonException("error.issue.type.not.existed");
        }
        PageConfigVO pageConfigVO = objectSchemeFieldService.listConfigs(organizationId, projectId, issueTypeId);
        PageTemplateVO result = new PageTemplateVO();
        //设置IssueTypeField
        pageConfigVO.setIssueTypeFieldVO(pageConfigVO.getIssueTypeFieldVO());
        //设置fields
        List<PageTemplateFieldVO> pageTemplateFieldList = modelMapper.map(pageConfigVO.getFields(), new TypeToken<List<PageTemplateFieldVO>>(){}.getType());
        result.setFields(pageTemplateFieldList);

        //设置级联说明
        setPageTemplateFieldCascadeRuleDes(issueTypeId, projectId, pageTemplateFieldList);
        return result;
    }

    private void setPageTemplateFieldCascadeRuleDes(Long issueTypeId, Long projectId, List<PageTemplateFieldVO> pageTemplateFieldList) {
        List<FieldCascadeRuleDesVO> fieldCascadeRuleDesList = fieldCascadeRuleMapper.selectFieldCascadeRuleDesByIssueTypeId(issueTypeId, projectId);
        Map<Long, List<FieldCascadeRuleDesVO>> cascadeRuleMap = fieldCascadeRuleDesList.stream().collect(Collectors.groupingBy(FieldCascadeRuleDesVO::getFieldId));
        pageTemplateFieldList.forEach(pageTemplateFieldVO -> pageTemplateFieldVO.setFieldCascadeRuleDesList(cascadeRuleMap.get(pageTemplateFieldVO.getFieldId())));
    }
}
