package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.PersonalTemplateCreateVO;
import io.choerodon.agile.api.vo.PersonalTemplatelVO;
import io.choerodon.agile.api.vo.PersonalTemplateUpdateVO;
import io.choerodon.agile.app.service.PersonalTemplateService;
import io.choerodon.agile.infra.dto.PersonalTemplateDTO;
import io.choerodon.agile.infra.mapper.PersonalTemplateMapper;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:56:32
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PersonalTemplateServiceImpl implements PersonalTemplateService {

    @Autowired
    private PersonalTemplateMapper personalTemplateMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public PersonalTemplatelVO create(Long projectId, PersonalTemplateCreateVO createVO) {
        if (checkNameByAction(projectId, createVO.getUserId(), createVO.getName(), createVO.getAction(), createVO.getType())) {
            throw new CommonException("error.personal.template.name.exist");
        }
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
        if (!Objects.equals(personalTemplateDTO.getName(), updateVO.getName())
                && checkNameByAction(projectId, personalTemplateDTO.getUserId(), updateVO.getName(), personalTemplateDTO.getAction(), personalTemplateDTO.getType())) {
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
        PersonalTemplateDTO check = new PersonalTemplateDTO(null, action, type, projectId, userId);
        check.setName(name);
        PersonalTemplateDTO personalTemplateDTO = personalTemplateMapper.selectOne(check);
        return !Objects.isNull(personalTemplateDTO);
    }
}
