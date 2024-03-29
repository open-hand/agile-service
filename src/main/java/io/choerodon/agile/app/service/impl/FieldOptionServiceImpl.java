package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.infra.mapper.FieldCascadeRuleMapper;
import io.choerodon.agile.infra.mapper.FieldCascadeRuleOptionMapper;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldExtendMapper;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.agile.api.vo.FieldOptionUpdateVO;
import io.choerodon.agile.api.vo.FieldOptionVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;
import io.choerodon.agile.app.service.FieldOptionService;
import io.choerodon.agile.app.service.FieldValueService;
import io.choerodon.agile.infra.dto.FieldOptionDTO;
import io.choerodon.agile.infra.mapper.FieldOptionMapper;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
@Service
public class FieldOptionServiceImpl implements FieldOptionService {
    private static final String ERROR_OPTION_ILLEGAL = "error.option.illegal";
    private static final String ERROR_OPTION_CREATE = "error.option.create";
    private static final String ERROR_OPTION_DELETE = "error.option.delete";
    private static final String ERROR_OPTION_NOTFOUND = "error.option.notFound";
    private static final String ERROR_OPTION_UPDATE = "error.option.update";
    @Autowired
    private FieldOptionMapper fieldOptionMapper;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired
    private FieldCascadeRuleOptionMapper fieldCascadeRuleOptionMapper;
    @Autowired
    private FieldCascadeRuleMapper fieldCascadeRuleMapper;

    @Override
    public FieldOptionDTO baseCreate(FieldOptionDTO option) {
        if (fieldOptionMapper.insert(option) != 1) {
            throw new CommonException(ERROR_OPTION_CREATE);
        }
        return fieldOptionMapper.selectByPrimaryKey(option.getId());
    }

    @Override
    public void baseDelete(Long optionId) {
        if (fieldOptionMapper.deleteByPrimaryKey(optionId) != 1) {
            throw new CommonException(ERROR_OPTION_DELETE);
        }
    }

    @Override
    public void baseUpdate(FieldOptionDTO option) {
        if (fieldOptionMapper.updateByPrimaryKeySelective(option) != 1) {
            throw new CommonException(ERROR_OPTION_UPDATE);
        }
    }

    @Override
    public FieldOptionDTO baseQueryById(Long organizationId, Long optionId) {
        FieldOptionDTO option = fieldOptionMapper.selectByPrimaryKey(optionId);
        if (option == null) {
            throw new CommonException(ERROR_OPTION_NOTFOUND);
        }
        if (!option.getOrganizationId().equals(organizationId)) {
            throw new CommonException(ERROR_OPTION_ILLEGAL);
        }
        return option;
    }

    @Override
    public synchronized String handleFieldOption(Long organizationId, Long fieldId, List<FieldOptionUpdateVO> newOptions) {
        List<FieldOptionVO> oldOptions = queryByFieldId(organizationId, fieldId);
        if (newOptions == null || oldOptions == null) {
            throw new CommonException(ERROR_OPTION_ILLEGAL);
        }
        //重名校验
        if (newOptions.stream().map(FieldOptionUpdateVO::getValue).collect(Collectors.toSet()).size() != newOptions.size()) {
            throw new CommonException(ERROR_OPTION_ILLEGAL);
        }
        if (newOptions.stream().map(FieldOptionUpdateVO::getCode).collect(Collectors.toSet()).size() != newOptions.size()) {
            throw new CommonException(ERROR_OPTION_ILLEGAL);
        }
        //删除校验
        List<Long> oldIds = oldOptions.stream().map(FieldOptionVO::getId).collect(Collectors.toList());
        List<Long> newIds = newOptions.stream().map(FieldOptionUpdateVO::getId).collect(Collectors.toList());
        List<Long> deleteIds = new ArrayList<>(oldIds);
        deleteIds.removeAll(newIds);
        fieldValueService.deleteByOptionIds(fieldId, deleteIds);
        //先删除所有选项
        deleteByFieldId(organizationId, fieldId);
        //设置排序
        AtomicInteger seq = new AtomicInteger(0);
        newOptions.forEach(option -> option.setSequence(seq.getAndIncrement()));
        //处理增加
        newOptions.stream().filter(x -> "add".equals(x.getStatus())).forEach(addOption -> {
            addOption.setId(null);
            create(organizationId, fieldId, addOption);
        });
        //处理修改与未修改
        newOptions.stream().filter(x -> !"add".equals(x.getStatus())).forEach(updateOption -> {
            if (updateOption.getId() == null) {
                throw new CommonException(ERROR_OPTION_ILLEGAL);
            }
            create(organizationId, fieldId, updateOption);
        });
        //处理默认值
        return newOptions.stream().filter(x -> x.getIsDefault() != null && x.getIsDefault()).map(x -> x.getId().toString()).collect(Collectors.joining(","));
    }


    @Override
    public List<FieldOptionVO> queryByFieldId(Long organizationId, Long fieldId) {
        return modelMapper.map(fieldOptionMapper.selectByFieldId(organizationId, fieldId), new TypeToken<List<FieldOptionVO>>() {
        }.getType());
    }

    @Override
    public void create(Long organizationId, Long fieldId, FieldOptionUpdateVO optionDTO) {
        FieldOptionDTO fieldOption = modelMapper.map(optionDTO, FieldOptionDTO.class);
        fieldOption.setOrganizationId(organizationId);
        fieldOption.setFieldId(fieldId);
        baseCreate(fieldOption);
        optionDTO.setId(fieldOption.getId());
    }

    @Override
    public void deleteByFieldId(Long organizationId, Long fieldId) {
        FieldOptionDTO delete = new FieldOptionDTO();
        delete.setFieldId(fieldId);
        delete.setOrganizationId(organizationId);
        fieldOptionMapper.delete(delete);
    }

    @Override
    public void fillOptions(Long organizationId, Long projectId, List<PageFieldViewVO> pageFieldViews) {
        List<Long> fieldIds = pageFieldViews.stream().map(PageFieldViewVO::getFieldId).collect(Collectors.toList());
        if (fieldIds != null && !fieldIds.isEmpty()) {
            List<FieldOptionVO> options = modelMapper.map(fieldOptionMapper.selectByFieldIds(organizationId, fieldIds), new TypeToken<List<FieldOptionVO>>() {
            }.getType());
            Map<Long, List<FieldOptionVO>> optionGroup = options.stream().collect(Collectors.groupingBy(FieldOptionVO::getFieldId));
            pageFieldViews.forEach(view -> {
                view.setFieldOptions(optionGroup.get(view.getFieldId()));
                String[] ids = String.valueOf(view.getDefaultValue()).split(",");
                List<Long> defaultIds = Arrays.asList((Long[]) ConvertUtils.convert(ids, Long.class));
                if (!CollectionUtils.isEmpty(defaultIds) && !CollectionUtils.isEmpty(view.getFieldOptions())) {
                    List<Long> newDefaultIds = new ArrayList<>();
                    List<Long> optionIds = view.getFieldOptions().stream().map(FieldOptionVO::getId).collect(Collectors.toList());
                    defaultIds.forEach(id -> {
                        if (optionIds.contains(id)) {
                            newDefaultIds.add(id);
                        }
                    });
                    view.setDefaultValue(StringUtils.join(newDefaultIds, ","));
                }
            });
        }
    }

    @Override
    public Page<FieldOptionVO> getOptionsPageByFieldId(Long organizationId, Long fieldId, String searchValue, List<Long> selected, Boolean enabled, PageRequest pageRequest) {
        Page<FieldOptionDTO> optionPage = PageHelper.doPage(pageRequest, () -> fieldOptionMapper.selectByFieldIdAndValue(organizationId, fieldId, searchValue, selected, enabled));
        if (CollectionUtils.isNotEmpty(selected) && pageRequest.getPage() == 0) {
            List<FieldOptionDTO> selectedOption = fieldOptionMapper.selectByOptionIds(organizationId, selected);
            if (CollectionUtils.isNotEmpty(selectedOption)) {
                selectedOption.addAll(optionPage.getContent());
                optionPage.setContent(selectedOption);
            }
        }
        return PageUtils.copyPropertiesAndResetContent(
                optionPage,
                modelMapper.map(optionPage.getContent(), new TypeToken<List<FieldOptionVO>>() {}.getType()));
    }

    @Override
    public FieldOptionVO insertOption(FieldOptionUpdateVO fieldOptionUpdateVO, Long fieldId, Long organizationId) {
        if (StringUtils.isBlank(fieldOptionUpdateVO.getValue()) || StringUtils.isBlank(fieldOptionUpdateVO.getCode())) {
            throw new CommonException(ERROR_OPTION_ILLEGAL);
        }
        List<FieldOptionDTO> fieldOptions = fieldOptionMapper.selectByCodeOrValue(organizationId, fieldId, fieldOptionUpdateVO.getCode(), fieldOptionUpdateVO.getValue());
        if (CollectionUtils.isNotEmpty(fieldOptions)) {
            throw new CommonException(ERROR_OPTION_ILLEGAL);
        }
        updateSequence(fieldOptionUpdateVO, null, fieldId, organizationId);
        create(organizationId, fieldId, fieldOptionUpdateVO);
        return modelMapper.map(fieldOptionUpdateVO, FieldOptionVO.class);
    }

    @Override
    public FieldOptionVO updateOption(FieldOptionUpdateVO fieldOptionUpdateVO, Long fieldId, Long organizationId) {
        FieldOptionDTO fieldOption = fieldOptionMapper.selectByPrimaryKey(fieldOptionUpdateVO.getId());
        if (fieldOption == null) {
            throw new CommonException(ERROR_OPTION_NOTFOUND);
        }
        List<FieldOptionDTO> fieldOptions = fieldOptionMapper.selectByCodeOrValue(organizationId, fieldId, fieldOptionUpdateVO.getCode(), fieldOptionUpdateVO.getValue());
        if (CollectionUtils.isNotEmpty(fieldOptions) &&
                (fieldOptions.size() > 1 || !ObjectUtils.equals(fieldOptions.get(0).getId(), fieldOptionUpdateVO.getId()))){
            throw new CommonException(ERROR_OPTION_ILLEGAL);
        }
        if (fieldOptionUpdateVO.getSequence() == null) {
            fieldOptionUpdateVO.setSequence(fieldOption.getSequence());
        }
        if (!ObjectUtils.equals(fieldOptionUpdateVO.getSequence(), fieldOption.getSequence())) {
            updateSequence(fieldOptionUpdateVO, fieldOption, fieldId, organizationId);
        }
        fieldOption.setCode(fieldOptionUpdateVO.getCode());
        fieldOption.setValue(fieldOptionUpdateVO.getValue());
        fieldOption.setEnabled(fieldOptionUpdateVO.getEnabled());
        fieldOption.setSequence(fieldOptionUpdateVO.getSequence());
        baseUpdate(fieldOption);
        return modelMapper.map(fieldOption, FieldOptionVO.class);
    }

    @Override
    public void deleteOption(Long optionId, Long fieldId, Long organizationId) {
        FieldOptionDTO fieldOption = fieldOptionMapper.selectByPrimaryKey(optionId);
        if (fieldOption == null) {
            throw new CommonException(ERROR_OPTION_NOTFOUND);
        }
        updateSequence(null, fieldOption, fieldId, organizationId);
        // 删除选项是不是默认值，以及清除级联规则
        handleDefaultValueAndCascadeRule(fieldId, optionId, organizationId);
        baseDelete(optionId);
    }

    private void handleDefaultValueAndCascadeRule(Long fieldId, Long optionId, Long organizationId) {
        objectSchemeFieldExtendMapper.updateDefaultValueDeleteOption(fieldId, optionId, organizationId);
        // 删除级联配置
        List<Long> cascadeRuleIds = fieldCascadeRuleOptionMapper.selectFieldCascadeRuleByFieldIdAndOptionId(organizationId, fieldId, optionId);
        if (CollectionUtils.isNotEmpty(cascadeRuleIds)) {
            fieldCascadeRuleMapper.deleteByCascadeRuleIds(cascadeRuleIds);
            fieldCascadeRuleOptionMapper.deleteByCascadeRuleIds(cascadeRuleIds);
        }
    }

    private void updateSequence(FieldOptionUpdateVO fieldOption, FieldOptionDTO oldFieldOption, Long fieldId, Long organizationId) {
        if (fieldOption == null && oldFieldOption == null) {
            return;
        }
        FieldOptionDTO optionRecord = new FieldOptionDTO();
        optionRecord.setFieldId(fieldId);
        optionRecord.setOrganizationId(organizationId);
        int count = fieldOptionMapper.selectCount(optionRecord);
        int newSequence = fieldOption == null || fieldOption.getSequence() >= count ? count - 1 : fieldOption.getSequence();
        int oldSequence = oldFieldOption == null ? count : oldFieldOption.getSequence();

        if (oldFieldOption == null && (fieldOption.getSequence() == null || fieldOption.getSequence() >= count)) {
            fieldOption.setSequence(count);
            return;
        }
        if (fieldOption != null){
            fieldOption.setSequence(newSequence);
        }

        if (newSequence > oldSequence) {
            fieldOptionMapper.sequenceDecrement(oldSequence, newSequence, fieldId, organizationId);
        } else if(newSequence < oldSequence){
            fieldOptionMapper.sequenceIncrement(newSequence, oldSequence, fieldId, organizationId);
        }
    }
}
