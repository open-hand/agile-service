package io.choerodon.agile.infra.enums;

/**
 * @author superlee
 * @since 2021-11-10
 */
public enum PredecessorType {
    /**
     * 结束到结束
     */
    PREDECESSOR_FF,
    /**
     * 结束到开始
     */
    PREDECESSOR_FS,
    /**
     * 开始到开始
     */
    PREDECESSOR_SS,
    /**
     * 开始到结束
     */
    PREDECESSOR_SF;

    public static boolean contains(String value) {
        for (PredecessorType predecessorType : PredecessorType.values()) {
            if (predecessorType.name().equalsIgnoreCase(value)) {
                return true;
            }
        }
        return false;
    }
}
