package io.choerodon.agile.api.vo.report;

import java.util.List;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import org.hzero.core.base.BaseConstants;
import org.springframework.util.Assert;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午7:02
 */
public class StaticListUnitVO extends ReportUnitVO {
    
    @Override
    public void validateAndconvert() {
        super.validateAndconvert();
        Assert.notNull(searchVO, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(searchVO.getOtherArgs(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(searchVO.getOtherArgs().get("issueIds"), BaseConstants.ErrorCode.DATA_INVALID);
        EncryptionUtils.decryptSearchVO(searchVO);
    }

    private List<String> colList;

    private SearchVO searchVO;

    public List<String> getColList() {
        return colList;
    }

    public void setColList(List<String> colList) {
        this.colList = colList;
    }

    public SearchVO getSearchVO() {
        return searchVO;
    }

    public void setSearchVO(SearchVO searchVO) {
        this.searchVO = searchVO;
    }
}
