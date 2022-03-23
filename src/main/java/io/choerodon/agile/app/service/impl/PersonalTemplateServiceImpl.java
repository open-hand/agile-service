package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.PersonalTemplateCreateVO;
import io.choerodon.agile.api.vo.PersonalTemplatelVO;
import io.choerodon.agile.api.vo.PersonalTemplateUpdateVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.app.service.PersonalTemplateService;
import io.choerodon.agile.infra.dto.PersonalTemplateDTO;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.mapper.PersonalTemplateMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:56:32
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PersonalTemplateServiceImpl implements PersonalTemplateService {

    private static final String[] AGILE_PERSONAL_TEMPLATE_ACTIONS = {"agile_import_issue","agile_export_issue","agile_export_feature", "agile_export_backlog","agile_import_backlog"};
    private static final String[] PROGRAM_PERSONAL_TEMPLATE_ACTIONS = {"program_import_feature","program_export_feature", "program_export_backlog","program_import_backlog"};
    private static final String[] PERSONAL_TEMPLATE_TYPES = {"excel"};
    @Autowired
    private PersonalTemplateMapper personalTemplateMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public PersonalTemplatelVO create(Long projectId, PersonalTemplateCreateVO createVO) {
        Boolean checkNameByAction = checkNameByAction(projectId, createVO.getUserId(), createVO.getName(), createVO.getAction(), createVO.getType());
        if (Boolean.TRUE.equals(checkNameByAction)) {
            throw new CommonException("error.personal.template.name.exist");
        }
        //校验action合法性
        checkAction(projectId, createVO.getAction());
        //校验类型
        checkType(createVO.getType());
        PersonalTemplateDTO personalTemplateDTO = modelMapper.map(createVO, PersonalTemplateDTO.class);
        if (personalTemplateMapper.insert(personalTemplateDTO) != 1) {
            throw new CommonException("error.personal.template.create");
        }
        return modelMapper.map(personalTemplateMapper.selectByPrimaryKey(personalTemplateDTO.getId()), PersonalTemplatelVO.class);
    }

    @Override
    public PersonalTemplatelVO update(Long projectId, Long id, PersonalTemplateUpdateVO updateVO) {
        PersonalTemplateDTO personalTemplateDTO = selectOne(projectId, id);
        if (Objects.isNull(personalTemplateDTO)) {
            throw new CommonException("error.personal.template.not.existed");
        }
        //校验当前用户
        checkUser(personalTemplateDTO.getUserId());
        Boolean checkNameByAction = checkNameByAction(projectId, personalTemplateDTO.getUserId(), updateVO.getName(), personalTemplateDTO.getAction(), personalTemplateDTO.getType());
        if (!Objects.equals(personalTemplateDTO.getName(), updateVO.getName())
                && Boolean.TRUE.equals(checkNameByAction)) {
            throw new CommonException("error.personal.template.name.exist");
        }
        PersonalTemplateDTO updateDTO = modelMapper.map(updateVO, PersonalTemplateDTO.class);
        if (personalTemplateMapper.updateByPrimaryKeySelective(updateDTO) != 1) {
            throw new CommonException("error.personal.template.update");
        }

        return modelMapper.map(personalTemplateMapper.selectByPrimaryKey(id), PersonalTemplatelVO.class);
    }

    @Override
    public void delete(Long projectId, Long id) {
        PersonalTemplateDTO personalTemplateDTO = selectOne(projectId, id);
        if (Objects.isNull(personalTemplateDTO)) {
            return;
        }
        //校验当前用户
        checkUser(personalTemplateDTO.getUserId());
        if (personalTemplateMapper.deleteByPrimaryKey(id) != 1) {
            throw new CommonException("error.personal.template.delete");
        }

    }

    private PersonalTemplateDTO selectOne(Long projectId, Long id) {
        PersonalTemplateDTO search = new PersonalTemplateDTO(id, null, null, projectId, null);
        return personalTemplateMapper.selectOne(search);
    }

    @Override
    public List<PersonalTemplatelVO> queryByUserAndAction(Long projectId, Long userId, String action, String type) {
        //校验action合法性
        checkAction(projectId, action);
        //校验类型
        checkType(type);
        PersonalTemplateDTO search = new PersonalTemplateDTO(null, action, type, projectId, userId);
        List<PersonalTemplateDTO> personalTemplateDTOList = personalTemplateMapper.select(search);
        if (CollectionUtils.isEmpty(personalTemplateDTOList)) {
            return new ArrayList<>();
        } else {
            return modelMapper.map(personalTemplateDTOList, new TypeToken<List<PersonalTemplatelVO>>(){}.getType());
        }
    }

    @Override
    public Boolean checkNameByAction(Long projectId, Long userId, String name, String action, String type) {
        //校验action合法性
        checkAction(projectId, action);
        //校验类型
        checkType(type);
        PersonalTemplateDTO check = new PersonalTemplateDTO(null, action, type, projectId, userId);
        check.setName(name);
        PersonalTemplateDTO personalTemplateDTO = personalTemplateMapper.selectOne(check);
        return !Objects.isNull(personalTemplateDTO);
    }

    private void checkAction(Long projectId, String action) {
        ProjectVO project = ConvertUtil.queryProject(projectId);
        List<String> actions = new ArrayList<>();
        List<String> categories = ProjectCategory.getProjectCategoryCodes(project);
        boolean containsAgile =
                categories.contains(ProjectCategory.MODULE_AGILE) || categories.contains(ProjectCategory.MODULE_WATERFALL_AGILE);
        boolean containsProgram = categories.contains(ProjectCategory.MODULE_PROGRAM);
        if (containsAgile) {
            actions.addAll(Arrays.asList(AGILE_PERSONAL_TEMPLATE_ACTIONS));
        }
        if (containsProgram) {
            actions.addAll(Arrays.asList(PROGRAM_PERSONAL_TEMPLATE_ACTIONS));
        }
        if (!actions.contains(action)) {
            throw new CommonException("error.action.is.illegal");
        }
    }

    private void checkUser(Long userId) {
        Long currentUserId = DetailsHelper.getUserDetails().getUserId();
        if (!Objects.equals(currentUserId, userId)) {
            throw new CommonException("error.template.not.belong.current.user");
        }
    }

    private void checkType(String type) {
        if (!Arrays.asList(PERSONAL_TEMPLATE_TYPES).contains(type)) {
            throw new CommonException("error.template.type.is.illegal");
        }
    }
}
