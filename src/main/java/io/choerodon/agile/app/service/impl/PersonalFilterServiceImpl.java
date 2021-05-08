package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.PersonalFilterVO;
import io.choerodon.agile.app.service.PersonalFilterService;
import io.choerodon.agile.app.service.QuickFilterFieldService;
import io.choerodon.agile.infra.dto.PersonalFilterDTO;
import io.choerodon.agile.infra.mapper.PersonalFilterMapper;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/2/25
 */
@Service
public class PersonalFilterServiceImpl implements PersonalFilterService {

    @Autowired
    private PersonalFilterMapper personalFilterMapper;

    public static final String UPDATE_ERROR = "error.personalFilter.update";
    public static final String DELETE_ERROR = "error.personalFilter.deleteById";
    public static final String NOTFOUND_ERROR = "error.personalFilter.notFound";
    public static final String NAME_ERROR = "error.personalFilter.nameNotNull";
    public static final String INSERT_ERROR = "error.personalFilter.create";
    public static final String NAME_EXIST = "error.personalFilter.nameExist";
    @Autowired
    private ModelMapper modelMapper;


    @Override
    public PersonalFilterVO queryById(Long projectId, Long filterId) {
        PersonalFilterDTO personalFilter = new PersonalFilterDTO();
        personalFilter.setFilterId(filterId);
        personalFilter.setProjectId(projectId);
        PersonalFilterDTO personalFilterDTO = personalFilterMapper.selectByPrimaryKey(personalFilter);
        if (personalFilterDTO == null) {
            throw new CommonException(NOTFOUND_ERROR);
        }
        personalFilterDTO.setFilterJson(EncryptionUtils.handlerPersonFilterJson(personalFilterDTO.getFilterJson(),true));
        PersonalFilterVO personalFilterVO = modelMapper.map(personalFilterDTO, PersonalFilterVO.class);
        return personalFilterVO;
    }

    @Override
    public PersonalFilterVO create(Long projectId, PersonalFilterVO personalFilterVO) {
        if (personalFilterVO.getName() == null || personalFilterVO.getName().equals("")) {
            throw new CommonException(NAME_ERROR);
        }
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        Long userId = customUserDetails.getUserId();
        if (checkName(projectId, userId, personalFilterVO.getName())) {
            throw new CommonException(NAME_EXIST);
        }
        if (!ObjectUtils.isEmpty(personalFilterVO.getDefault()) && Boolean.TRUE.equals(personalFilterVO.getDefault())) {
            personalFilterMapper.updateDefault(projectId, userId, false, null);
        }
        personalFilterVO.setUserId(userId);
        personalFilterVO.setProjectId(projectId);
        personalFilterVO.setFilterJson(EncryptionUtils.handlerPersonFilterJson(personalFilterVO.getFilterJson(),false));
        PersonalFilterDTO personalFilterDTO = modelMapper.map(personalFilterVO, PersonalFilterDTO.class);
        if (personalFilterMapper.insert(personalFilterDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        return queryById(projectId, personalFilterDTO.getFilterId());
    }

    @Override
    public PersonalFilterVO update(Long projectId, Long filterId, PersonalFilterVO personalFilterVO) {
        personalFilterVO.setFilterId(filterId);
        PersonalFilterDTO personalFilterDTO = modelMapper.map(personalFilterVO, PersonalFilterDTO.class);
        personalFilterDTO.setFilterJson(EncryptionUtils.handlerPersonFilterJson(personalFilterDTO.getFilterJson(),false));
        if (!ObjectUtils.isEmpty(personalFilterVO.getDefault()) && Boolean.TRUE.equals(personalFilterVO.getDefault())) {
            Long userId = DetailsHelper.getUserDetails().getUserId();
            personalFilterMapper.updateDefault(projectId, userId, false, null);
        }
        if (personalFilterMapper.updateByPrimaryKeySelective(personalFilterDTO) != 1) {
            throw new CommonException(UPDATE_ERROR);
        }
        return queryById(projectId, filterId);
    }

    @Override
    public void deleteById(Long projectId, Long filterId) {
        PersonalFilterDTO personalFilterDTO = new PersonalFilterDTO();
        personalFilterDTO.setProjectId(projectId);
        personalFilterDTO.setFilterId(filterId);
        int isDelete = personalFilterMapper.delete(personalFilterDTO);
        if (isDelete != 1) {
            throw new CommonException(DELETE_ERROR);
        }
    }

    @Override
    public List<PersonalFilterVO> listByProjectId(Long projectId, Long userId, String searchStr) {
        List<PersonalFilterVO> list = modelMapper.map(personalFilterMapper.queryByProjectIdAndUserId(projectId, userId, searchStr), new TypeToken<List<PersonalFilterVO>>() {
        }.getType());
        list.forEach(v -> v.setFilterJson(EncryptionUtils.handlerPersonFilterJson(v.getFilterJson(),true)));
        return list;
    }

    @Override
    public Boolean checkName(Long projectId, Long userId, String name) {
        PersonalFilterDTO personalFilterDTO = new PersonalFilterDTO();
        personalFilterDTO.setProjectId(projectId);
        personalFilterDTO.setUserId(userId);
        personalFilterDTO.setName(name);
        List<PersonalFilterDTO> list = personalFilterMapper.select(personalFilterDTO);
        return list != null && !list.isEmpty();
    }

    @Override
    public Boolean setDefault(Long projectId, Long filterId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        personalFilterMapper.updateDefault(projectId, userId, false, null);
        int result = personalFilterMapper.updateDefault(projectId, userId, true, filterId);
        return result > 0;
    }
}
