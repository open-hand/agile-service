package io.choerodon.agile.api.validator;

import io.choerodon.agile.infra.dto.ProductVersionDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.ProductVersionMapper;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Objects;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/6/3.
 * Email: fuqianghuang01@gmail.com
 */
@Component
public class StoryMapValidator {

    private static final String ARCHIVED = "archived";
    private static final String RELEASED = "released";

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private ProductVersionMapper productVersionMapper;

    public void checkEpicExist(Long epicId) {
        if (epicId != null && !Objects.equals(epicId, 0L) && issueMapper.selectByPrimaryKey(epicId) == null) {
            throw new CommonException("error.epic.notFound");
        }
    }

    public void checkVersionExist(Long versionId) {
        if (versionId != null && !Objects.equals(versionId, 0L)) {
            ProductVersionDTO productVersionDTO = productVersionMapper.selectByPrimaryKey(versionId);
            if (productVersionDTO == null) {
                throw new CommonException("error.version.notFound");
            }
            if (ARCHIVED.equals(productVersionDTO.getStatusCode()) || RELEASED.equals(productVersionDTO.getStatusCode())) {
                throw new CommonException("error.productStatus.notRight");
            }
        }
    }

}
