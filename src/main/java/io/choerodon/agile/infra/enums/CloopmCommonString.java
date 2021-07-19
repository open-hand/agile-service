package io.choerodon.agile.infra.enums;

/**
 * @author cong.cheng@hand-china.com
 */
public class CloopmCommonString {
    private CloopmCommonString() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * 是否可被删除
     */
    public static final String CAN_DELETE = "canDelete";


    public static final String PRIORITY = "priority";
    public static final String COMPONENT = "component";
    public static final String INFLUENCE_VERSION = "influenceVersion";
    public static final String FIX_VERSION = "fixVersion";
    public static final String PROGRAM_VERSION = "programVersion";

}
